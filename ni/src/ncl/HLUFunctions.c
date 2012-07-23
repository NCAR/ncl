#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/DataComm.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/DataItem.h>
#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/ResourcesP.h>
#include "defs.h"
#include "Symbol.h"
#include <regex.h>

#include "NclDataDefs.h"
#include "NclMdInc.h"
#include "TypeSupport.h"
#include "NclHLUObj.h"
#include "NclBuiltInSupport.h"
#include "Machine.h"
#include "HLUSupport.h"
#include "HluClasses.h"


NhlErrorTypes _NclINhlIsApp
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsApp(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}

NhlErrorTypes _NclINhlIsDataComm
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsDataComm(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclINhlIsDataItem
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsDataItem(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclINhlIsDataSpec
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsDataSpec(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclINhlIsTransform
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsTransform(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclINhlIsView
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsView(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclINhlIsWorkstation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	logical *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(type == NCL_obj) {
		if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				} else {
					tmp_hlu_ptr[i] = NULL;
				}
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}
		}
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				outpt[i] = (NhlIsWorkstation(tmp_hlu_ptr[i]->hlu.hlu_id)?1:0);
			} else {
				outpt[i] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			}
		}
		NclFree(tmp_hlu_ptr);
	} else {
		outpt = (logical*)NclMalloc(sizeof(logical)*total);
		for( i = 0; i < total; i++) {
			outpt[i] = 0;
		}
	}
	
	return(NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
		NCL_logical,
                0
        ));
}
NhlErrorTypes _NclIChangeWorkstation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_wk;
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_wk;
	obj *ncl_hlu_obj_ids;
	obj *wk_obj_id;
	NclHLUObj wk_ptr;
	NhlErrorTypes ret = NhlNOERROR;
	NclHLUExpChildList *exp_list;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	wk_obj_id = (obj*)NclGetArgValue(
                        1,
                        nargs,
                        NULL,
                        NULL,
                        &missing_wk,
                        &has_missing_wk,
                        NULL,
			0);

	if((has_missing_wk)&&(*wk_obj_id == missing.objval)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIChangeWorkstation: workstation parameter is a missing value");
		return(NhlFATAL);
	} else {
		wk_ptr = (NclHLUObj)_NclGetObj(*wk_obj_id);
		if(wk_ptr != NULL) {
			if(!_NhlIsWorkstation(_NhlGetLayer(wk_ptr->hlu.hlu_id))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIChangeWorkstation: workstation parameter does not exist as an HLU workstation");
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIChangeWorkstation: workstation parameter does not exist as an HLU workstation");
			return(NhlFATAL);
		}
	}


	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	for( i = 0; i < j; i++) {
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsView(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			if(tmp_hlu_ptr[i]->hlu.exp_list != NULL) {
/*
* All exported objects will be moved to the new workstation by the HLU's
* This is hear to assure NCL keeps track of this fact.
*/
				exp_list = tmp_hlu_ptr[i]->hlu.exp_list;
				while(exp_list != NULL) {
					if(tmp_hlu_ptr[i]->hlu.parent_hluobj_id != -1) {
						_NclDelHLUChild((NclHLUObj)_NclGetObj(tmp_hlu_ptr[i]->hlu.parent_hluobj_id),exp_list->child_id);
					}
					_NclAddHLUChild(wk_ptr,exp_list->child_id);
					exp_list = exp_list->next;
				}
			}
			if(tmp_hlu_ptr[i]->hlu.parent_hluobj_id != -1) {
				_NclDelHLUChild((NclHLUObj)_NclGetObj(tmp_hlu_ptr[i]->hlu.parent_hluobj_id),tmp_hlu_ptr[i]->obj.id);
			}
			_NclAddHLUChild(wk_ptr,tmp_hlu_ptr[i]->obj.id);
			if(NhlChangeWorkstation(tmp_hlu_ptr[i]->hlu.hlu_id,wk_ptr->hlu.hlu_id) < NhlNOERROR) {
				ret = NhlWARNING;
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIChangeWorkstation: one of the elements of the plot parameter is not a View object, ignoring it");
			ret = NhlWARNING;
		}
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclISetColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 5;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int n_dims_c;
	ng_size_t dimsizes_c[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type_c;
    ng_size_t total=1;
    ng_size_t total_c =1;
    ng_size_t i,j=0,k;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_c;
	NclScalar missing_r;
	NclScalar missing_g;
	NclScalar missing_b;
	int has_missing_c;
	int has_missing_r;
	int has_missing_g;
	int has_missing_b;
	NhlErrorTypes ret = NhlNOERROR;
	
	obj *ncl_hlu_obj_ids;
	int *color_inds;
	float *red;
	float *green;
	float *blue;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	color_inds = (int*)NclGetArgValue(
		1,
		nargs,
		&n_dims,
		dimsizes,
		&missing,
		&has_missing_c,
		&type,
		0);
	for(i = 0; i < n_dims; i++) {
		total_c *= dimsizes[i];
	}

	red = (float*)NclGetArgValue(
		2,
		nargs,
		&n_dims_c,
		dimsizes_c,
		&missing_r,
		&has_missing_r,
		&type_c,
		0);
	if(n_dims_c != n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension mismatch, number of dimensions 1-4 must match");
		return(NhlFATAL);
	} else {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes_c[i] != dimsizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension size mismatch, dimension sizes of arguments 1-4 must match");
				return(NhlFATAL);
			}
		}
	}
	green = (float*)NclGetArgValue(
		3,
		nargs,
		&n_dims_c,
		dimsizes_c,
		&missing_g,
		&has_missing_g,
		&type_c,
		0);
	if(n_dims_c != n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension mismatch, number of dimensions 1-4 must match");
		return(NhlFATAL);
	} else {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes_c[i] != dimsizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension size mismatch, dimension sizes of arguments 1-4 must match");
				return(NhlFATAL);
			}
		}
	}
	blue = (float*)NclGetArgValue(
		4,
		nargs,
		&n_dims_c,
		dimsizes_c,
		&missing_b,
		&has_missing_b,
		&type_c,
		0);
	if(n_dims_c != n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension mismatch, number of dimensions 1-4 must match");
		return(NhlFATAL);
	} else {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes_c[i] != dimsizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension size mismatch, dimension sizes of arguments 1-4 must match");
				return(NhlFATAL);
			}
		}
	}

	for( i = 0; i < j; i++) {
		
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			if((!has_missing_c)||(color_inds[i] != missing_c.intval)) {
				for(k = 0; k < total_c; k++) {
					if( ((!has_missing_r)||(red[i] != missing_r.floatval))&&
					((!has_missing_g)||(green[i] != missing_g.floatval))&&
					((!has_missing_b)||(blue[i] != missing_b.floatval))) {
						ret = NhlSetColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k],red[k],green[k],blue[k]);
					}
					if(ret < NhlNOERROR) {
						ret = NhlWARNING;
					}
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclISetColor: one of the elements of the workstation parameter does not exist as an HLU workstation, ignoring it");
			ret = NhlWARNING;
		}
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclINewColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int n_dims_c;
	ng_size_t dimsizes_c[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type_c;
    ng_size_t total=1;
    ng_size_t total_c =1;
	int m;
    ng_size_t i,j=0,k;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_r;
	NclScalar missing_g;
	NclScalar missing_b;
	int has_missing_r;
	int has_missing_g;
	int has_missing_b;
	obj *ncl_hlu_obj_ids;
	int *colori_out;
	float *red;
	float *green;
	float *blue;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			}  else {
				tmp_hlu_ptr[i] = NULL;
			}
		}
		j = total;
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}

	red = (float*)NclGetArgValue(
		1,
		nargs,
		&n_dims,
		dimsizes,
		&missing_r,
		&has_missing_r,
		&type_c,
		0);
	for(i = 0; i < n_dims; i++) {
		total_c *= dimsizes[i];
	}
	green = (float*)NclGetArgValue(
		2,
		nargs,
		&n_dims_c,
		dimsizes_c,
		&missing_g,
		&has_missing_g,
		&type_c,
		0);
	if(n_dims_c != n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NewColor: parameter dimension mismatch, number of dimensions 1-4 must match");
		return(NhlFATAL);
	} else {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes_c[i] != dimsizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NewColor: parameter dimension size mismatch, dimension sizes of arguments 1-4 must match");
				return(NhlFATAL);
			}
		}
	}
	blue = (float*)NclGetArgValue(
		3,
		nargs,
		&n_dims_c,
		dimsizes_c,
		&missing_b,
		&has_missing_b,
		&type_c,
		0);
	if(n_dims_c != n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension mismatch, number of dimensions 1-4 must match");
		return(NhlFATAL);
	} else {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes_c[i] != dimsizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"SetColor: parameter dimension size mismatch, dimension sizes of arguments 1-4 must match");
				return(NhlFATAL);
			}
		}
	}

	colori_out = (int*)NclMalloc(sizeof(int)*j*total_c);
	for( i = 0; i < j; i++) {
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			m = 0;
			for(k = 0; k < total_c; k++) {
					if( ((!has_missing_r)||(red[i] != missing_r.floatval))&&
					((!has_missing_g)||(green[i] != missing_g.floatval))&&
					((!has_missing_b)||(blue[i] != missing_b.floatval))) {
						colori_out[i*j+k] = NhlNewColor(tmp_hlu_ptr[i]->hlu.hlu_id,red[k],green[k],blue[k]);
					} else {
						colori_out[i*j+k] = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
					}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclINewColor: one of the elements of the workstation parameter does not exist as an HLU workstation, ignoring");
			for(k = 0; k < total_c; k++) {
				colori_out[i*j+k] = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
			}
		}
	}
	if(j == 1) {
		n_dims = 1;
		dimsizes[0] = total_c;
	} else {
		n_dims = 2;
		dimsizes[0] = j;
		dimsizes[1] = total_c;
	}
	NclFree(tmp_hlu_ptr);
	return(NclReturnValue(
                colori_out,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypeintClass)->type_class.default_mis),
                NCL_int,
                0
        ));
}
NhlErrorTypes _NclIFreeColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t total_c =1;
    ng_size_t i,j=0,k;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_c;
	int has_missing_c;
	NhlErrorTypes ret = NhlNOERROR;
	obj *ncl_hlu_obj_ids;
	int *color_inds;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	color_inds = (int*)NclGetArgValue(
		1,
		nargs,
		&n_dims,
		dimsizes,
		&missing,
		&has_missing_c,
		&type,
		0);
	for(i = 0; i < n_dims; i++) {
		total_c *= dimsizes[i];
	}

	for( i = 0; i < j; i++) {
		
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			for(k = 0; k < total_c; k++) {
				if((!has_missing_c)||(color_inds[k] != missing_c.intval)) {
					ret = NhlFreeColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k]);
				}
				if(ret < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIFreeColor: one of the elements of the workstation parameter does not exist as an HLU workstation, ignoring it");
			ret = NhlWARNING;
		}
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIIsAllocatedColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t total_c =1;
    ng_size_t i,j=0,k;
	logical *log_out;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_c;
	int has_missing_c;
	obj *ncl_hlu_obj_ids;
	int *color_inds;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL; 
			}
		}
		j = total;
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	color_inds = (int*)NclGetArgValue(
		1,
		nargs,
		&n_dims,
		dimsizes,
		&missing,
		&has_missing_c,
		&type,
		0);
	for(i = 0; i < n_dims; i++) {
		total_c *= dimsizes[i];
	}

	log_out = (logical*)NclMalloc(sizeof(logical)*j*total_c);
	for( i = 0; i < j; i++) {
		
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			for(k = 0; k < total_c; k++) {
				if((!has_missing_c)||(color_inds[k] != missing_c.intval)) {
					log_out[i*total_c+k] = NhlIsAllocatedColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k]);
				} else {
					log_out[i*total_c+k] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
				}
			} 
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsAllocatedColor: one of the elements of the workstation parameter does not exist as an HLU workstation, ignoring");
			for(k = 0; k < total_c; k++) {
				log_out[i*total_c+k] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
			} 
		}
	}
	if(j == 1) {
		n_dims = 1;
		dimsizes[0] = total_c;
	} else {
		n_dims = 2;
		dimsizes[0] = j;
		dimsizes[1] = total_c;
	}
	NclFree(tmp_hlu_ptr);
	return(NclReturnValue(
                (void*)log_out,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypelogicalClass)->type_class.default_mis),
                NCL_logical,
                0
        ));
}
NhlErrorTypes _NclIGetBB
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	NhlBoundingBox the_box;
	float *out_val;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL;
			}
			
		}
		j = total ;
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total ;
	}
	out_val = (float*)NclMalloc(sizeof(float)*j*4);
	for( i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			if( NhlGetBB(tmp_hlu_ptr[i]->hlu.hlu_id,&the_box) > NhlWARNING) {
				out_val[i*4] = the_box.t;
				out_val[i*4+1] = the_box.b;
				out_val[i*4+2] = the_box.l;
				out_val[i*4+3] = the_box.r;
			} else {
				out_val[i*4] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
				out_val[i*4+1] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
				out_val[i*4+2] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
				out_val[i*4+3] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			}
		} else {
			out_val[i*4] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*4+1] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*4+2] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*4+3] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		}
	}
	if(total == 1) {
		n_dims = 1;
		dimsizes[0] = 4;
	} else {
		n_dims = 2;
		dimsizes[0] = j;
		dimsizes[1] = 4;
	}
	NclFree(tmp_hlu_ptr);
	return(NclReturnValue(
                (void*)out_val,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypefloatClass)->type_class.default_mis),
		NCL_float,
                0
        ));
}
NhlErrorTypes _NclIAddData      
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type2;
    ng_size_t total=1;
    ng_size_t total2=1;
    ng_size_t i,j=0,k=0,l;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_data_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	string *resname;
	obj *ncl_data_obj_ids;
	int n_dims_ =1;
	ng_size_t  len_dims[2];
	struct _NclHLUObjRec *tmp_hlu;
	NclStackEntry data_out;
	obj *out_dspec_ids;
	int tmp;
	NhlLayer tmp_layer;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);
	resname = (string*)NclGetArgValue(
			1,
			nargs,
			NULL,
			NULL,
			&missing1,
			&has_missing1,
			NULL,
			0);
	ncl_data_obj_ids = (obj*)NclGetArgValue(
			2,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			&type2,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL;
			}
		}
		j = total;
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	for(i = 0; i < n_dims2; i++) {
		total2 *= dimsizes2[i];
	}
	tmp_data_ptr  = (NclHLUObj*)NclMalloc(dimsizes2[0]*sizeof(NclHLUObj));
	if(has_missing2) {
		for( i = 0; i < total2; i++) {
			if(ncl_data_obj_ids[i] != missing2.objval) {
				tmp_data_ptr[i] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
			} else {
				tmp_data_ptr[i] = NULL;
			}
		}
		k = total2;
	} else {
		for( i = 0; i < total; i++) {
			tmp_data_ptr[i] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
		}
		k = total2;
	}
	out_dspec_ids = (obj*)NclMalloc(k*j*sizeof(obj));
	for(i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			for(l = 0; l < k ; l++) {
				if(tmp_data_ptr[l] != NULL) {
					tmp = NhlAddData(tmp_hlu_ptr[i]->hlu.hlu_id,NrmQuarkToString(*resname),tmp_data_ptr[l]->hlu.hlu_id);

					if(tmp > 0) {
						_NclAddHLUToExpList(tmp_hlu_ptr[i],tmp_data_ptr[l]->obj.id);
						tmp_layer = _NhlGetLayer(tmp);
						if(tmp_layer != NULL) {
                                                	tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
							_NclAddHLUChild(tmp_hlu_ptr[i],tmp_hlu->obj.id);
							out_dspec_ids[i*j+l] = tmp_hlu->obj.id;
						} else {
							out_dspec_ids[i*j+l] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
						}
                                        } else {
						out_dspec_ids[i*j+l] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
                                        }
					if(tmp < 0) {
						ret = NhlWARNING;
					}
				} else {
					out_dspec_ids[i*j+l] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				}
			}
		} else {
			for(l = 0; l < k ; l++) {
				out_dspec_ids[i*j+l] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;	
			}
		}
	}
	if(j == 1)  {
		n_dims_ = 1;
		len_dims[0] = k;
	} else if(k == 1) {
		n_dims_ = 1;
		len_dims[0] = j;
	} else {
		n_dims_ = 2;
		len_dims[0] = j;
		len_dims[1] = k;
	}
        data_out.kind = NclStk_VAL;
        data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
        	NULL,NULL, Ncl_MultiDValHLUObjData,
                0,(void*)out_dspec_ids,&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
                len_dims,TEMPORARY,NULL);
	_NclPlaceReturn(data_out);
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_data_ptr);
	return(ret);
}
NhlErrorTypes _NclIRemoveData
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type2;
    ng_size_t total=1;
    ng_size_t total2=1;
    ng_size_t i,j=0,k=0,l;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_data_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	string *resname;
	obj *ncl_data_obj_ids;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);
	resname = (string*)NclGetArgValue(
			1,
			nargs,
			NULL,
			NULL,
			&missing1,
			&has_missing1,
			NULL,
			0);
	ncl_data_obj_ids = (obj*)NclGetArgValue(
			2,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			&type2,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	for(i = 0; i < n_dims2; i++) {
		total2 *= dimsizes2[i];
	}
	tmp_data_ptr  = (NclHLUObj*)NclMalloc(total2*sizeof(NclHLUObj));
	if(has_missing2) {
		for( i = 0; i < total2; i++) {
			if(ncl_data_obj_ids[i] != missing2.objval) {
				tmp_data_ptr[k] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
				k++;
			}
		}
	} else {
		for( i = 0; i < total2; i++) {
			tmp_data_ptr[i] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
		}
		k = total2;
	}
	for(i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			for(l = 0; l < k ; l++) {
				if(tmp_data_ptr[l] != NULL) {
					if(NhlRemoveData(tmp_hlu_ptr[i]->hlu.hlu_id,NrmQuarkToString(*resname),tmp_data_ptr[l]->hlu.hlu_id) < NhlNOERROR) {
						ret = NhlWARNING;
					}
				}
			}
		}
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_data_ptr);
	return(ret);
}
NhlErrorTypes _NclIRemoveOverlay
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	NclBasicDataTypes type1;
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj overlay_obj_ptr;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *ncl_plot_obj_ids;
	logical *restore;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			NULL,
			NULL,
			NULL,
			0);
	ncl_plot_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			&type1,
			0);
	restore = (obj*)NclGetArgValue(
			2,
			nargs,
			NULL,
			NULL,
			&missing2,
			&has_missing2,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_plot_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_plot_obj_ids[i]);
				j++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_plot_obj_ids[i]);
		}
		j = total;
	}
	overlay_obj_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_ids);
	if(overlay_obj_ptr != NULL) {
		for( i = 0; i < j; i++) {
			if(tmp_hlu_ptr[i] != NULL ) {
				if(NhlRemoveOverlay(overlay_obj_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id,(has_missing2 ? ( missing2.logicalval == *restore ? 0 : *restore) : *restore)) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIAddToOverlay2
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int has_missing2;
	NclBasicDataTypes type1;
	ng_size_t total=1;
	ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *base_hlu_obj_id;
	obj *after_hlu_obj_id;
	
	base_hlu_obj_id = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			&missing,
			&has_missing,
			NULL,
			0);
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			&type1,
			0);
	after_hlu_obj_id = (obj*)NclGetArgValue(
			2,
			nargs,
			NULL,
			NULL,
			&missing2,
			&has_missing2,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	for( i = 0; i < j; i++) {
		
	}
	return NhlNOERROR;
}
NhlErrorTypes _NclIAddAnnotation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	ng_size_t total=1, len_dims = 1;
	ng_size_t i,j=0;
	int n_dims_ = 1;
	int tmp;
	obj *out_anno_ids;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_base_ptr;
	struct _NclHLUObjRec *tmp_hlu;
	NclScalar missing;
	NclScalar missing1;
	obj *ncl_hlu_obj_ids;
	obj *ncl_ano_obj_ids;
	NclStackEntry data_out;
	NhlLayer tmp_layer;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			&missing,
			&has_missing,
			NULL,
			0);

	ncl_ano_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_ano_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_ano_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL;
			}
		}
		j = total;
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_ano_obj_ids[i]);
		}
		j = total;
	}
	out_anno_ids = (obj*)NclMalloc(j*sizeof(obj));
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_ids)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_ids);
		if(tmp_base_ptr != NULL) {
			for( i = 0; i < j; i++) {
				if((tmp_hlu_ptr[i] != NULL )&&(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)!=NULL)) { 
					tmp= NhlAddAnnotation(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id);

					if(tmp > 0) {
						_NclAddHLUToExpList(tmp_base_ptr,tmp_hlu_ptr[i]->obj.id);
						tmp_layer = _NhlGetLayer(tmp);
						if(tmp_layer != NULL) {
							tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
							_NclAddHLUChild(tmp_base_ptr,tmp_hlu->obj.id);
							out_anno_ids[i] =  tmp_hlu->obj.id;
						} else {
							out_anno_ids[i] =  ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
						}
					} else {
						out_anno_ids[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
					}
					if(tmp<0) {
						ret = NhlWARNING;
					}
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlAddAnnotation: bad HLU id passed in, ignoring it");
				}
			}
		} else {
			for( i = 0; i < j; i++) {
				out_anno_ids[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
			}
		}
	}  else {
		for( i = 0; i < j; i++) {
			out_anno_ids[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
		}
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIAddAnnotation: First parameter is a missing value, returning missing values");
		ret = NhlWARNING;
	}
	n_dims_  = 1;
	len_dims = j;
	data_out.kind = NclStk_VAL;
	data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
		NULL,NULL, Ncl_MultiDValHLUObjData,
		0,(void*)out_anno_ids,&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
		&len_dims,TEMPORARY,NULL);
	_NclPlaceReturn(data_out);
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIRemoveAnnotation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_base_ptr;
	NclScalar missing;
	NclScalar missing1;
	obj *ncl_hlu_obj_ids;
	obj *ncl_ano_obj_ids;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			&missing,
			&has_missing,
			NULL,
			0);

	ncl_ano_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_ano_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_ano_obj_ids[i]);
		}
		j = total;
	}
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_ids)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_ids);
		if(tmp_base_ptr != NULL) {
			for( i = 0; i < j; i++) {
				if(tmp_hlu_ptr[i] != NULL ) { 
					if(NhlRemoveAnnotation(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id) < NhlNOERROR){
						ret = NhlWARNING;
					}
				}
			}
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIRemoveAnnotation: First parameter is a missing value, returning missing values");
		NclFree(tmp_hlu_ptr);
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}

NhlErrorTypes _NclIAddPrimitive
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing,has_missing1,has_missing2;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_before_ptr;
	NclHLUObj tmp_base_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_id;
	obj *ncl_prim_obj_ids;
	obj *ncl_before_obj_id;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_id = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			&missing,
			&has_missing,
			NULL,
			0);

	ncl_prim_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	ncl_before_obj_id = (obj*)NclGetArgValue(
			2,
			nargs,
			NULL,
			NULL,
			&missing2,
			&has_missing2,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_prim_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_prim_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_prim_obj_ids[i]);
		}
		j = total;
	}
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_id)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_id);
		if(tmp_base_ptr != NULL) {
			int before_id;
			if (ncl_before_obj_id && (! has_missing2 || missing2.objval != *ncl_before_obj_id)) {
				tmp_before_ptr = (NclHLUObj)_NclGetObj(*ncl_before_obj_id);
				before_id = tmp_before_ptr == NULL ? 0 : tmp_before_ptr->hlu.hlu_id;
			}
			else {
				before_id = 0;
			}
			for( i = 0; i < j; i++) {
				if(tmp_hlu_ptr[i] != NULL ) { 
					if(NhlAddPrimitive(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id,before_id) < NhlNOERROR){
						ret = NhlWARNING;
					}
				}
			}
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIRemovePrimitive: First parameter is a missing value, returning missing values");
		NclFree(tmp_hlu_ptr);
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}

NhlErrorTypes _NclIRemovePrimitive
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_base_ptr;
	NclScalar missing;
	NclScalar missing1;
	obj *ncl_hlu_obj_id;
	obj *ncl_prim_obj_ids;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_id = (obj*)NclGetArgValue(
			0,
			nargs,
			NULL,
			NULL,
			&missing,
			&has_missing,
			NULL,
			0);

	ncl_prim_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	for(i = 0; i < n_dims1; i++) {
		total *= dimsizes1[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing1) {
		for( i = 0; i < total; i++) {
			if(ncl_prim_obj_ids[i] != missing1.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_prim_obj_ids[i]);
				j++;
			} 
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_prim_obj_ids[i]);
		}
		j = total;
	}
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_id)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_id);
		if(tmp_base_ptr != NULL) {
			for( i = 0; i < j; i++) {
				if(tmp_hlu_ptr[i] != NULL ) { 
					if(NhlRemovePrimitive(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id) < NhlNOERROR){
						ret = NhlWARNING;
					}
				}
			}
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIRemovePrimitive: First parameter is a missing value, returning missing values");
		NclFree(tmp_hlu_ptr);
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}

NhlErrorTypes _NclIUpdateData
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				j++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	for( i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			if(NhlUpdateData(tmp_hlu_ptr[i]->hlu.hlu_id) < NhlNOERROR) {
				ret = NhlWARNING;
			}
		}
	}
	NclFree(tmp_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIDataPolymarker
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t total_=1;
    ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlDataPolymarker(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIDataPolygon
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t total_=1;
    ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlDataPolygon(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclIDataPolyline 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t total_=1;
    ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlDataPolyline(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclINDCPolygon
#if     NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t total_=1;
    ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolygon: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolygon: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolygon: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolygon: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlNDCPolygon(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolygon: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclINDCPolymarker
#if     NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t total_=1;
	ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolymarker: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolymarker: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolymarker: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolymarker: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlNDCPolymarker(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolymarker: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}
NhlErrorTypes _NclINDCPolyline 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_;
	int n_dims_;
	ng_size_t dimsizes_[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int n_dims1;
	ng_size_t dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	int n_dims2;
	ng_size_t dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t total_=1;
	ng_size_t i,j=0,k=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_style_hlu_ptr;
	NclScalar missing;
	NclScalar missing_;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *style_hlu_obj_ids;
	float *x;
	float *y;
	NhlErrorTypes ret = NhlNOERROR;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	style_hlu_obj_ids = (obj*)NclGetArgValue(
			1,
			nargs,
			&n_dims_,
			dimsizes_,
			&missing_,
			&has_missing_,
			&type,
			0);

	x = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			3,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
			0);

	
	total *= dimsizes[0];
	total_ *= dimsizes_[0];

	if((total == total_)||(total_ == 1)){
		tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
		tmp_style_hlu_ptr  = (NclHLUObj*)NclMalloc(total_*sizeof(NclHLUObj));
		if((has_missing)&&(has_missing_)) {
			if(total_ != 1) {
				for( i = 0; i < total; i++) {
					if((ncl_hlu_obj_ids[i] != missing.objval)&&(style_hlu_obj_ids[i] != missing_.objval)) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			} else {
				if(style_hlu_obj_ids[0] == missing_.objval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolyLine: a missing value for the style object was detected, can't perform draw");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
				for( i = 0; i < total; i++) {
					if(ncl_hlu_obj_ids[i] != missing.objval) {
						tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
						if(total_ > 1) {
							tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
						}
						j++;
					}
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else if(has_missing) {
			for( i = 0; i < total; i++) {
				if(ncl_hlu_obj_ids[i] != missing.objval) {
					tmp_hlu_ptr[j] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
					if(total_ > 1) {
						tmp_style_hlu_ptr[j] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
					}
					j++;
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = j;
			}
		} else {
			for( i = 0; i < total; i++) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(total_ > 1) {
					tmp_style_hlu_ptr[i] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[i]);
				}
			}
			if(total_ == 1) {
				tmp_style_hlu_ptr[0] = (NclHLUObj)_NclGetObj(style_hlu_obj_ids[0]);
				k = 1;
			} else {
				k = total;
			}
		
			j = total;
		}
		if(dimsizes1[0] != dimsizes2[0]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolyLine: x and y parameters must have the same dimension size");
			NclFree(tmp_hlu_ptr);
			NclFree(tmp_style_hlu_ptr);
			return(NhlWARNING);
		}
		if(has_missing1){
			for( i = 0; i < n_dims1; i++) {
				if(x[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolyLine: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		if(has_missing2){
			for( i = 0; i < n_dims2; i++) {
				if(y[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NDCPolyLine: missing value detected,  x and y parameters must not contain any missing values");
					NclFree(tmp_hlu_ptr);
					NclFree(tmp_style_hlu_ptr);
					return(NhlWARNING);
				}
			}
		}
		for( i = 0; i < j; i++) {
			if((tmp_hlu_ptr[i] != NULL)&&(((total_ == 1)&&(tmp_style_hlu_ptr[0] != NULL))||(tmp_style_hlu_ptr[i] != NULL))) {
				if(NhlNDCPolyline(tmp_hlu_ptr[i]->hlu.hlu_id,((total_ == 1)? tmp_style_hlu_ptr[0]->hlu.hlu_id:tmp_style_hlu_ptr[i]->hlu.hlu_id),x,y,dimsizes1[0]) < NhlNOERROR) {
					ret = NhlWARNING;
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: The must either be one style object or the same number of style objects as plots");
		return(NhlWARNING);
	}
	NclFree(tmp_hlu_ptr);
	NclFree(tmp_style_hlu_ptr);
	return(ret);
}

NhlErrorTypes _NclIClassName 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	string *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
	}
	outpt = (string*)NclMalloc(sizeof(string)*total);
	for( i = 0; i < total; i++) {
		if(tmp_hlu_ptr[i] != NULL ) {
			outpt[i] = NrmStringToQuark(NhlClassName(tmp_hlu_ptr[i]->hlu.hlu_id));
		} else {
			outpt[i] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		}
	}
	NclFree(tmp_hlu_ptr);
	return(NclReturnValue(
                (void*)outpt,
                1,
                dimsizes,
                &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
		NCL_string,
                0
        ));
}
NhlErrorTypes _NclIName
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t i;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	string *outpt;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	tmp_hlu_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total; i++) {
			if(ncl_hlu_obj_ids[i] != missing.objval) {
				tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			} else {
				tmp_hlu_ptr[i] = NULL;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
	}
	outpt = (string*)NclMalloc(sizeof(string)*total);
	for( i = 0; i < total; i++) {
		if(tmp_hlu_ptr[i] != NULL ) {
			outpt[i] = NrmStringToQuark(NhlName(tmp_hlu_ptr[i]->hlu.hlu_id));
		} else {
			outpt[i] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		}
	}
	NclFree(tmp_hlu_ptr);
	return(NclReturnValue(
                (void*)outpt,
                1,
                dimsizes,
                &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
		NCL_string,
                0
        ));
}
NhlErrorTypes _NclINhlGetWorkspaceObjectId
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int n_dims_ = 1;
	ng_size_t len_dims = 1;
	int tmp;
	obj *out_ids;
	struct _NclHLUObjRec *tmp_hlu;
	NclStackEntry data_out;
	NhlErrorTypes ret = NhlNOERROR;
	NhlLayer tmp_layer;

	tmp = NhlGetWorkspaceObjectId();
	tmp_layer = _NhlGetLayer(tmp);
	tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
	out_ids = (obj*)NclMalloc(sizeof(obj));
	*out_ids = tmp_hlu->obj.id;
	data_out.kind = NclStk_VAL;
	data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
		NULL,NULL, Ncl_MultiDValHLUObjData,
		0,(void*)out_ids,&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
		&len_dims,TEMPORARY,NULL);
	_NclPlaceReturn(data_out);
	return(ret);
}
NhlErrorTypes _NclINhlAppGetDefaultParentId
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int n_dims_ = 1;
	ng_size_t len_dims = 1;
	int tmp;
	obj *out_ids;
	struct _NclHLUObjRec *tmp_hlu;
	NclStackEntry data_out;
	NhlErrorTypes ret = NhlNOERROR;
	NhlLayer tmp_layer;

	tmp = NhlAppGetDefaultParentId();
	if(tmp) {
		tmp_layer = _NhlGetLayer(tmp);
		tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
		out_ids = (obj*)NclMalloc(sizeof(obj));
		*out_ids = tmp_hlu->obj.id;
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
			NULL,NULL, Ncl_MultiDValHLUObjData,
			0,(void*)out_ids,&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
			&len_dims,TEMPORARY,NULL);
	} else {
		out_ids = (obj*)NclMalloc(sizeof(obj));
		*out_ids = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
			NULL,NULL, Ncl_MultiDValHLUObjData,
			0,(void*)out_ids,&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
			&len_dims,TEMPORARY,NULL);
	}
	_NclPlaceReturn(data_out);
	return(ret);
}
NhlErrorTypes _NclINhlGetParentWorkstation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	ng_size_t total=1;
	ng_size_t i;
	NclHLUObj tmp_hlu_ptr;
	NclHLUObj tmp_hlu;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	obj *outpt;
	int tmp_id;
	NclHLULookUpTable* tmp_lo;
	NhlErrorTypes ret = NhlNOERROR;


        ncl_hlu_obj_ids = (obj*)NclGetArgValue(
                        0,
                        nargs,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
 
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	outpt = NclMalloc(sizeof(obj)*total);
        if(has_missing) {
                for( i = 0; i < total; i++) {
                        if(ncl_hlu_obj_ids[i] != missing.objval) {
                                tmp_hlu_ptr = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(tmp_hlu_ptr != NULL) {
					tmp_id = NhlGetParentWorkstation(tmp_hlu_ptr->hlu.hlu_id);
					if(tmp_id < 1) {	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentWorkstation : Object does not have a parent workstation, it must not be drawable");
						ret = NhlWARNING;
						outpt[i] = missing.objval;
					} else {
						tmp_lo = _NclGetHLURefInfo(tmp_id);
						if(tmp_lo != NULL ) {
							outpt[i] = tmp_lo->ncl_hlu_id;
						} else {
							tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp_id,-1,_NhlClass(_NhlGetLayer(tmp_id)));
							if(tmp_hlu != NULL) {	
								outpt[i] = tmp_hlu->obj.id;
							} else {
								outpt[i] = missing.objval;
							}
						}
					}
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentWorkstation : Invalid plot passed in can't get objects parent workstation");
					ret = NhlWARNING;
					outpt[i] = missing.objval;
				}
                        } else {
                                tmp_hlu_ptr = NULL;
				outpt[i] = missing.objval;
                        }
                }
        } else {
                for( i = 0; i < total; i++) {
                        tmp_hlu_ptr = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			if(tmp_hlu_ptr != NULL) {
				tmp_id = NhlGetParentWorkstation(tmp_hlu_ptr->hlu.hlu_id);
				if(tmp_id < 1) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentWorkstation : Object does not have a parent workstation, it must not be drawable");
					ret = NhlWARNING;
					outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				} else {
					tmp_lo = _NclGetHLURefInfo(tmp_id);
					if(tmp_lo != NULL ) {
						outpt[i] = tmp_lo->ncl_hlu_id;
					} else {
						tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp_id,-1,_NhlClass(_NhlGetLayer(tmp_id)));
						if(tmp_hlu != NULL) {	
							outpt[i] = tmp_hlu->obj.id;
						} else {
							outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
						}
					}
				}
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentWorkstation : Invalid plot passed in can't get objects parent workstation");
				outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				ret = NhlWARNING;
			}
                }
        }
	NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                has_missing? &missing : &(((NclTypeClass)nclTypeobjClass)->type_class.default_mis),
		NCL_obj,
                0
        );
	return(ret);
}

NhlErrorTypes _NclINhlGetParentId
#if	NhlNeedProto
(void)
#else
()
#endif
{
    int nargs = 1;
    int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type;
    ng_size_t total=1;
    ng_size_t i;
    NclHLUObj tmp_hlu_ptr;
    NclScalar missing;
    obj *ncl_hlu_obj_ids;
    obj *outpt;
	int tmp_id;
	NclHLULookUpTable* tmp_lo;
	NhlErrorTypes ret = NhlNOERROR;
	NclHLUObj tmp_hlu;


        ncl_hlu_obj_ids = (obj*)NclGetArgValue(
                        0,
                        nargs,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
 
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	outpt = NclMalloc(sizeof(obj)*total);
        if(has_missing) {
                for( i = 0; i < total; i++) {
                        if(ncl_hlu_obj_ids[i] != missing.objval) {
                                tmp_hlu_ptr = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
				if(tmp_hlu_ptr != NULL) {
					tmp_id = NhlGetParentId(tmp_hlu_ptr->hlu.hlu_id);
					if(tmp_id < 1) {	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentId: Object does not have a parent");
						ret = NhlWARNING;
						outpt[i] = missing.objval;
					} else {
						tmp_lo = _NclGetHLURefInfo(tmp_id);
						if(tmp_lo != NULL ) {
							outpt[i] = tmp_lo->ncl_hlu_id;
						} else {
							tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp_id,-1,_NhlClass(_NhlGetLayer(tmp_id)));
							if(tmp_hlu != NULL) {	
								outpt[i] = tmp_hlu->obj.id;
							} else {
								outpt[i] = missing.objval;
							}
						}
					}
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentId: Invalid plot passed in can't get objects parent");
					ret = NhlWARNING;
					outpt[i] = missing.objval;
				}
                        } else {
                                tmp_hlu_ptr = NULL;
				outpt[i] = missing.objval;
                        }
                }
        } else {
                for( i = 0; i < total; i++) {
                        tmp_hlu_ptr = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
			if(tmp_hlu_ptr != NULL) {
				tmp_id = NhlGetParentId(tmp_hlu_ptr->hlu.hlu_id);
				if(tmp_id < 1) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentId: Object does not have a parent");
					ret = NhlWARNING;
					outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				} else {
					tmp_lo = _NclGetHLURefInfo(tmp_id);
					if(tmp_lo != NULL ) {
						outpt[i] = tmp_lo->ncl_hlu_id;
					} else {
						tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp_id,-1,_NhlClass(_NhlGetLayer(tmp_id)));
						if(tmp_hlu != NULL) {	
							outpt[i] = tmp_hlu->obj.id;
						} else {
							outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
						}
					}
				}
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetParentId: Invalid plot passed in can't get objects parent");
				outpt[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
				ret = NhlWARNING;
			}
                }
        }
	NclReturnValue(
                (void*)outpt,
                n_dims,
                dimsizes,
                has_missing? &missing : &(((NclTypeClass)nclTypeobjClass)->type_class.default_mis),
		NCL_obj,
                0
        );
	return(ret);
}

NhlErrorTypes _NclINhlPalGetDefined
#if	NhlNeedProto
(void)
#else
()
#endif
{
	char **names;
	NclQuark *output;
	ng_size_t num;
	ng_size_t i;

	num = NhlPalGetDefined(NhlworkstationClass,&names);
	output = (NclQuark*)NclMalloc(sizeof(NclQuark)*num);
	for(i = 0; i < num; i++) {
		output[i] = NrmStringToQuark(names[i]);
	}
	NclReturnValue(
                (void*)output,
                1,
                &num,
		NULL,
		NCL_string,
                0
        );
	return NhlNOERROR;
}

NhlErrorTypes _NclISetDashPattern
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing0,has_missing1,has_missing2;
	ng_size_t size0=1,size1=1,size2=1;
	NclScalar missing0;
	NclScalar missing1;
	NclScalar missing2;
	int nwks;
    ng_size_t i,j=0;
	NclHLUObj tmp_wks;
	obj *wks_obj_ids;
	int *dash_indexes;
	string *dash_patterns;
	NhlErrorTypes subret,ret = NhlNOERROR;
	

	wks_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing0,
			&has_missing0,
			NULL,
			0);
	size0 = dimsizes[0];

	dash_indexes = (int*)NclGetArgValue(
			1,
			nargs,
			&n_dims,
			dimsizes,
			&missing1,
			&has_missing1,
			NULL,
			0);
	size1 = dimsizes[0];

	dash_patterns = (string*)NclGetArgValue(
			2,
			nargs,
			&n_dims,
			dimsizes,
			&missing2,
			&has_missing2,
			NULL,
			0);
	size2 = dimsizes[0];

	nwks = 0;
	for (i = 0; i < size0; i++) {
		if (has_missing0 && wks_obj_ids[i] == missing0.objval)
			continue;
		tmp_wks = (NclHLUObj)_NclGetObj(wks_obj_ids[i]);
		if (tmp_wks == NULL)
			continue;
		nwks++;
		for( j = 0; j < size1; j++) {
			if (has_missing1 && (dash_indexes[j] == missing1.intval))
				continue;
			if (j < size2 && (! has_missing2 || (dash_patterns[j] != missing2.stringval))) {
				subret = NhlSetDashPattern(tmp_wks->hlu.hlu_id,
							   dash_indexes[j],NrmQuarkToString(dash_patterns[j]));
			}
			else {
				subret = NhlSetDashPattern(tmp_wks->hlu.hlu_id,dash_indexes[j],"");
			}
			ret = MIN(ret,subret);
			if (ret < NhlWARNING) {
				return ret;
			}
		}
	}
	if (nwks == 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclISetDashPattern: No valid workstation");
		return(NhlWARNING);
	}
	return(ret);
}

NhlErrorTypes _NclINewDashPattern
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing0,has_missing1;
	ng_size_t size0=1,size1=1;
	NclScalar missing0;
	NclScalar missing1;
	int nwks;
    ng_size_t i,j=0;
	NclHLUObj tmp_wks;
	obj *wks_obj_ids;
	string *dash_patterns;
	NhlErrorTypes subret,ret = NhlNOERROR;
	int *indexes = NULL;

	wks_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing0,
			&has_missing0,
			NULL,
			0);
	size0 = dimsizes[0];

	dash_patterns = (string*)NclGetArgValue(
			1,
			nargs,
			&n_dims,
			dimsizes,
			&missing1,
			&has_missing1,
			NULL,
			0);
	size1 = dimsizes[0];

	indexes = (int *)NclMalloc(size0 * size1 * sizeof(int));

	nwks = 0;
	for (i = 0; i < size0; i++) {
		int wks_is_missing = 0;
		if (has_missing0 && wks_obj_ids[i] == missing0.objval) {
			wks_is_missing = 1;
		}
		else { 
			tmp_wks = (NclHLUObj)_NclGetObj(wks_obj_ids[i]);
			if (tmp_wks == NULL || ! NhlIsWorkstation(tmp_wks->hlu.hlu_id)) {
				wks_is_missing = 1;
			}
			else {
				nwks++;
			}
		}
		for( j = 0; j < size1; j++) {
			int *index = indexes + i * size1 + j;
			if (wks_is_missing) {
				*index = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
				continue;
			}
			if (has_missing1 && (dash_patterns[j] == missing1.stringval)) {
				*index = NhlNewDashPattern(tmp_wks->hlu.hlu_id,"");
			}
			else {
				*index = NhlNewDashPattern(tmp_wks->hlu.hlu_id,NrmQuarkToString(dash_patterns[j]));
			}
			if (*index < 0) {
				subret = (NhlErrorTypes) *index;
				*index = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
			}
			ret = MIN(ret,subret);
			if (ret < NhlWARNING) {
				return ret;
			}
		}
	}
	if (nwks == 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclINewDashPattern: No valid workstation");
		ret = MIN(ret,NhlWARNING);
	}
	n_dims = 1;
	if (size0 > 1 && size1 > 1) {
		n_dims = 2;
		dimsizes[0] = size0;
		dimsizes[1] = size1;
	}
	else if (size0 > 1) {
		dimsizes[0] = size0;
	}
	else if (size1 > 1) {
		dimsizes[0] = size1;
	}
	else {
		dimsizes[0] = 1;
	}
	
	subret = NclReturnValue (
		(void*)indexes,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypeintClass)->type_class.default_mis),
		NCL_int,
		0
                );

	return(MIN(ret,subret));
}

double *coerce_to_double(
	void              *in,
	NclBasicDataTypes type_in,
	int               size_in,
	int               has_missing_in,
	NclScalar         *missing_in,
	NclScalar         *missing_out)
{
	double *out;

	if (type_in == NCL_double) {
		out = (double *)in;
		if (has_missing_in) {
			missing_out->doubleval = missing_in->doubleval;
		}
		return out;
	}
	out = (double *) NclMalloc(size_in * sizeof(double));
	if (! out) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	if (has_missing_in) {
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,
			   (void*)missing_out,
			   (void*)missing_in,
			   1,
			   NULL,
			   NULL,
			   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_in)));
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,
			   (void*)out,in,size_in,missing_in,missing_out,
			   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_in)));
	}
	else {
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,
			   (void*)out,in,size_in,NULL,NULL,
			   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_in)));
	}
	return out;
}

NhlErrorTypes _NclISetMarker
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 9;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing0,has_missing1,has_missing2,has_missing3,
		has_missing4,has_missing5,has_missing6,has_missing7,has_missing8;
	NclScalar missing0;
	NclScalar missing1;
	NclScalar missing2;
	NclScalar missing3;
	NclScalar missing4;
	NclScalar missing5;
	NclScalar missing6;
	NclScalar missing7;
	NclScalar missing8;
	NclBasicDataTypes type4,type5,type6,type7,type8;
	void *val4,*val5,*val6,*val7,*val8;
	ng_size_t size0,size1,size2,size3,size4,size5,size6,size7,size8;
	int nwks;
    ng_size_t i,j=0;
	NclHLUObj tmp_wks;
	obj *wks_obj_ids;
	int *marker_indexes;
	string *marker_strings;
	int   *m_font;
	double *m_x_off;
	double *m_y_off;
	double *m_aspect_adj;
	double *m_size_adj;
	double *m_angle;
	NclScalar m_x_off_missing, m_y_off_missing, m_aspect_adj_missing, m_size_adj_missing, m_angle_missing;
	NhlErrorTypes subret,ret = NhlNOERROR;
	
	wks_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing0,
			&has_missing0,
			NULL,
			0);
	size0 = dimsizes[0];

	marker_indexes = (int*)NclGetArgValue(
			1,
			nargs,
			&n_dims,
			dimsizes,
			&missing1,
			&has_missing1,
			NULL,
			0);
	size1 = dimsizes[0];

	marker_strings = (string*)NclGetArgValue(
			2,
			nargs,
			&n_dims,
			dimsizes,
			&missing2,
			&has_missing2,
			NULL,
			0);
	size2 = dimsizes[0];

	m_font = (int*)NclGetArgValue(
			3,
			nargs,
			&n_dims,
			dimsizes,
			&missing3,
			&has_missing3,
			NULL,
			0);
	size3 = dimsizes[0];

	val4 = (void*)NclGetArgValue(
			4,
			nargs,
			&n_dims,
			dimsizes,
			&missing4,
			&has_missing4,
			&type4,
			0);
	size4 = dimsizes[0];

	val5 = (void*)NclGetArgValue(
			5,
			nargs,
			&n_dims,
			dimsizes,
			&missing5,
			&has_missing5,
			&type5,
			0);
	size5 = dimsizes[0];

	val6 = (void*)NclGetArgValue(
			6,
			nargs,
			&n_dims,
			dimsizes,
			&missing6,
			&has_missing6,
			&type6,
			0);
	size6 = dimsizes[0];

	val7 = (void*)NclGetArgValue(
			7,
			nargs,
			&n_dims,
			dimsizes,
			&missing7,
			&has_missing7,
			&type7,
			0);
	size7 = dimsizes[0];

	val8 = (void*)NclGetArgValue(
			8,
			nargs,
			&n_dims,
			dimsizes,
			&missing8,
			&has_missing8,
			&type8,
			0);
	size8 = dimsizes[0];

	m_x_off = coerce_to_double(val4,type4,size4,has_missing4,&missing4,&m_x_off_missing);
	m_y_off = coerce_to_double(val5,type5,size5,has_missing5,&missing5,&m_y_off_missing);
	m_aspect_adj = coerce_to_double(val6,type6,size6,has_missing6,&missing6,&m_aspect_adj_missing);
	m_size_adj = coerce_to_double(val7,type7,size7,has_missing7,&missing7,&m_size_adj_missing);
	m_angle = coerce_to_double(val8,type8,size8,has_missing8,&missing8,&m_angle_missing);

	nwks = 0;
	for (i = 0; i < size0; i++) {
		if (has_missing0 && wks_obj_ids[i] == missing0.objval)
			continue;
		tmp_wks = (NclHLUObj)_NclGetObj(wks_obj_ids[i]);
		if (tmp_wks == NULL)
			continue;
		nwks++;
		for( j = 0; j < size1; j++) {
			double x_off,y_off,aspect_adj,size_adj,angle;
			int font;
			string marker_string;

			if (has_missing1 && (marker_indexes[j] == missing1.intval))
				continue;

			/* 
			 * If the marker_string is scalar it is used for all markers, otherwise if not
			 * available the default marker is used.
			 * Parameters that are scalar apply to all markers, otherwise if not available
			 * default values are used.
			 */

			if (size2 == 1 && ! (marker_strings[0] == missing2.stringval)) {
				marker_string = marker_strings[0];
			}
			else if (j >= size2 || (has_missing2 && (marker_strings[j] == missing2.stringval))) {
				subret = NhlSetMarker(tmp_wks->hlu.hlu_id,marker_indexes[j],"",0,0.0,0.0,0.0,0.0,0.0);
				continue;
			}
			else {
				marker_string = marker_strings[j];
			}

			if (size3 == 1 && !(has_missing3 && (m_font[0] == missing3.intval))) {
				font = m_font[0];
			}
			else {
				font = (j >= size3 || (has_missing3 && (m_font[j] == missing3.intval))) ?
					0 : m_font[j];
			}

			if (size4 == 1 && !(has_missing4 && (m_x_off[0] == m_x_off_missing.doubleval))) {
				x_off = m_x_off[0];
			}
			else {
				x_off =  (j >= size4 || (has_missing4 && (m_x_off[j] == m_x_off_missing.doubleval))) ?
					0.0 : m_x_off[j];
			}
			if (size5 == 1 && ! (has_missing5 && (m_y_off[0] == m_y_off_missing.doubleval))) {
				y_off =  m_y_off[0];
			}
			else {
				y_off =  (j >= size5 || (has_missing5 && (m_y_off[j] == m_y_off_missing.doubleval))) ?
					0.0 : m_y_off[j];
			}
			if (size6 == 1 && ! (has_missing6 && (m_aspect_adj[0] == m_aspect_adj_missing.doubleval))) {
				aspect_adj = m_aspect_adj[0];
			}
			else {
				aspect_adj = (j >= size6 || (has_missing6 && (m_aspect_adj[j] ==m_aspect_adj_missing.doubleval))) ?
					0.0 : m_aspect_adj[j];
			}
			if (size7 == 1 && ! (has_missing7 && (m_size_adj[0] == m_size_adj_missing.doubleval))) {
				size_adj = m_size_adj[0];
			}
			else {
				size_adj = (j >= size7 || (has_missing7 && (m_size_adj[j] == m_size_adj_missing.doubleval))) ?
					0.0 : m_size_adj[j];
			}
			if (size8 == 1 && ! (has_missing8 && (m_angle[0] == m_angle_missing.doubleval))) {
				angle = m_angle[0];
			}
			else {
				angle = (j >= size8 || (has_missing8 && (m_angle[j] == m_angle_missing.doubleval))) ?
					0.0 : m_angle[j];
			}
			subret = NhlSetMarker(tmp_wks->hlu.hlu_id,marker_indexes[j],NrmQuarkToString(marker_string),
					      font,(float)x_off,(float)y_off,(float)aspect_adj,(float)size_adj,(float)angle);
			ret = MIN(ret,subret);
			if (ret < NhlWARNING) {
				goto RETURN;
			}
		}
	}

 RETURN:
	if ((void*)m_x_off != val4)
		NclFree(m_x_off);
	if ((void*)m_y_off != val5)
		NclFree(m_y_off);
	if ((void*)m_aspect_adj != val6)
		NclFree(m_aspect_adj);
	if ((void*)m_size_adj != val7)
		NclFree(m_size_adj);
	if ((void*)m_angle != val8)
		NclFree(m_angle);
	
	if (nwks == 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclISetMarker: No valid workstation");
		return(NhlWARNING);
	}
	return(ret);
}

NhlErrorTypes _NclINewMarker
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 8;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing0,has_missing1,has_missing2,has_missing3,
		has_missing4,has_missing5,has_missing6,has_missing7;
	NclScalar missing0;
	NclScalar missing1;
	NclScalar missing2;
	NclScalar missing3;
	NclScalar missing4;
	NclScalar missing5;
	NclScalar missing6;
	NclScalar missing7;
	NclBasicDataTypes type3,type4,type5,type6,type7;
	void *val3,*val4,*val5,*val6,*val7;
	ng_size_t size0,size1,size2,size3,size4,size5,size6,size7;
	int nwks;
	ng_size_t i,j=0;
	NclHLUObj tmp_wks;
	obj *wks_obj_ids;
	string *marker_strings;
	int *m_font;
	double *m_x_off;
	double *m_y_off;
	double *m_aspect_adj;
	double *m_size_adj;
	double *m_angle;
	NclScalar m_x_off_missing, m_y_off_missing, m_aspect_adj_missing, m_size_adj_missing, m_angle_missing;
	NhlErrorTypes subret,ret = NhlNOERROR;
	int *indexes = NULL;

	wks_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing0,
			&has_missing0,
			NULL,
			0);
	size0 = dimsizes[0];

	marker_strings = (string*)NclGetArgValue(
			1,
			nargs,
			&n_dims,
			dimsizes,
			&missing1,
			&has_missing1,
			NULL,
			0);
	size1 = dimsizes[0];

	m_font = (int*)NclGetArgValue(
			2,
			nargs,
			&n_dims,
			dimsizes,
			&missing2,
			&has_missing2,
			NULL,
			0);
	size2 = dimsizes[0];

	val3 = (void*)NclGetArgValue(
			3,
			nargs,
			&n_dims,
			dimsizes,
			&missing3,
			&has_missing3,
			&type3,
			0);
	size3 = dimsizes[0];

	val4 = (void*)NclGetArgValue(
			4,
			nargs,
			&n_dims,
			dimsizes,
			&missing4,
			&has_missing4,
			&type4,
			0);
	size4 = dimsizes[0];

	val5 = (void*)NclGetArgValue(
			5,
			nargs,
			&n_dims,
			dimsizes,
			&missing5,
			&has_missing5,
			&type5,
			0);
	size5 = dimsizes[0];

	val6 = (void*)NclGetArgValue(
			6,
			nargs,
			&n_dims,
			dimsizes,
			&missing6,
			&has_missing6,
			&type6,
			0);
	size6 = dimsizes[0];

	val7 = (void*)NclGetArgValue(
			7,
			nargs,
			&n_dims,
			dimsizes,
			&missing7,
			&has_missing7,
			&type7,
			0);
	size7 = dimsizes[0];

	m_x_off = coerce_to_double(val3,type3,size3,has_missing3,&missing3,&m_x_off_missing);
	m_y_off = coerce_to_double(val4,type4,size4,has_missing4,&missing4,&m_y_off_missing);
	m_aspect_adj = coerce_to_double(val5,type5,size5,has_missing5,&missing5,&m_aspect_adj_missing);
	m_size_adj = coerce_to_double(val6,type6,size6,has_missing6,&missing6,&m_size_adj_missing);
	m_angle = coerce_to_double(val7,type7,size7,has_missing7,&missing7,&m_angle_missing);

	indexes = (int *)NclMalloc(size0 * size1 * sizeof(int));

	nwks = 0;
	for (i = 0; i < size0; i++) {
		int wks_is_missing = 0;
		if (has_missing0 && wks_obj_ids[i] == missing0.objval) {
			wks_is_missing = 1;
		}
		else { 
			tmp_wks = (NclHLUObj)_NclGetObj(wks_obj_ids[i]);
			if (tmp_wks == NULL || ! NhlIsWorkstation(tmp_wks->hlu.hlu_id)) {
				wks_is_missing = 1;
			}
			else {
				nwks++;
			}
		}
		for( j = 0; j < size1; j++) {
			int *index = indexes + i * size1 + j;
			if (wks_is_missing) {
				*index = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
				continue;
			}
			if (has_missing1 && (marker_strings[j] == missing1.stringval)) {
				*index = NhlNewMarker(tmp_wks->hlu.hlu_id,"",0,0.0,0.0,0.0,0.0,0.0);
			}
			else {
				double x_off,y_off,aspect_adj,size_adj,angle;
				int font;

				/* 
				 * Parameters that are scalar apply to all markers, otherwise if not available
				 * default values are used.
				 */

				if (size2 == 1 && !(has_missing2 && (m_font[0] == missing2.intval))) {
					font = m_font[0];
				}
				else {
					font = (j >= size2 || (has_missing2 && (m_font[j] == missing2.intval))) ?
						0 : m_font[j];
				}
				if (size3 == 1 && !(has_missing3 && (m_x_off[0] == m_x_off_missing.doubleval))) {
					x_off = m_x_off[0];
				}
				else {
					x_off = (j >= size3 || (has_missing3 && (m_x_off[j] == m_x_off_missing.doubleval))) ?
						0.0 : m_x_off[j];
				}
				if (size4 == 1 && ! (has_missing4 && (m_y_off[0] == m_y_off_missing.doubleval))) {
					y_off = m_y_off[0];
				}
				else {
					y_off = (j >= size4 || (has_missing4 && (m_y_off[j] == m_y_off_missing.doubleval))) ?
						0.0 : m_y_off[j];
				}
				if (size5 == 1 && ! (has_missing5 && (m_aspect_adj[0] == m_aspect_adj_missing.doubleval))) {
					aspect_adj = m_aspect_adj[0];
				}
				else {
					aspect_adj = (j >= size5 || 
						      (has_missing5 && (m_aspect_adj[j] == m_aspect_adj_missing.doubleval))) ?
						0.0 : m_aspect_adj[j];
				}
				if (size6 == 1 && ! (has_missing6 && (m_size_adj[0] == m_size_adj_missing.doubleval))) {
					size_adj = m_size_adj[0];
				}
				else {
					size_adj = (j >= size6 || 
						    (has_missing6 && (m_size_adj[j] == m_size_adj_missing.doubleval))) ?
						0.0 : m_size_adj[j];
				}
				if (size7 == 1 && ! (has_missing7 && (m_angle[0] == m_angle_missing.doubleval))) {
					angle = m_angle[0];
				}
				else {
					angle = (j >= size7 || (has_missing7 && (m_angle[j] == m_angle_missing.doubleval))) ?
						0.0 : m_angle[j];
				}
				*index = NhlNewMarker(tmp_wks->hlu.hlu_id,NrmQuarkToString(marker_strings[j]),font,
						      (float)x_off,(float)y_off,(float)aspect_adj,(float)size_adj,(float)angle);
			}
			if (*index < 0) {
				subret = (NhlErrorTypes) *index;
				*index = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
			}
			ret = MIN(ret,subret);
			if (ret < NhlWARNING) {
				goto RETURN;
			}
		}
	}

 RETURN:
	if ((void*)m_x_off != val3)
		NclFree(m_x_off);
	if ((void*)m_y_off != val4)
		NclFree(m_y_off);
	if ((void*)m_aspect_adj != val5)
		NclFree(m_aspect_adj);
	if ((void*)m_size_adj != val6)
		NclFree(m_size_adj);
	if ((void*)m_angle != val7)
		NclFree(m_angle);

	if (nwks == 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclINewMarker: No valid workstation");
		ret = MIN(ret,NhlWARNING);
	}
	n_dims = 1;
	if (size0 > 1 && size1 > 1) {
		n_dims = 2;
		dimsizes[0] = size0;
		dimsizes[1] = size1;
	}
	else if (size0 > 1) {
		dimsizes[0] = size0;
	}
	else if (size1 > 1) {
		dimsizes[0] = size1;
	}
	else {
		dimsizes[0] = 1;
	}

	subret = NclReturnValue (
		(void*)indexes,
                n_dims,
                dimsizes,
                &(((NclTypeClass)nclTypeintClass)->type_class.default_mis),
		NCL_int,
		0
                );

	return(MIN(ret,subret));
}

NhlErrorTypes _NclINhlGetErrorObjectId
#if NhlNeedProto
(void)
#else
()
#endif
{
    int n_dims_ = 1;
    ng_size_t len_dims = 1;
    int tmp;
    obj *out_ids;
    struct _NclHLUObjRec *tmp_hlu;
    NclStackEntry data_out;
    NhlErrorTypes ret = NhlNOERROR;
    NhlLayer tmp_layer;                                                                                                    
    tmp = NhlGetErrorObjectId();
    tmp_layer = _NhlGetLayer(tmp);
    tmp_hlu = _NclHLUObjCreate(NULL, NULL, Ncl_HLUObj, 0, STATIC,
                tmp, -1, tmp_layer->base.layer_class);
    out_ids = (obj *) NclMalloc(sizeof(obj));
    *out_ids = tmp_hlu->obj.id;
    data_out.kind = NclStk_VAL;
    data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
                NULL, NULL, Ncl_MultiDValHLUObjData,
                0, (void *) out_ids,
                &((NclTypeClass)nclTypeobjClass)->type_class.default_mis,n_dims_,
                &len_dims,TEMPORARY,NULL);
    _NclPlaceReturn(data_out);
    return ret;
}

NhlErrorTypes _NclINhlGetClassResources
#if	NhlNeedProto
(void)
#else
()
#endif
{
    int nargs = 2;
    int has_missing,n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    NclBasicDataTypes type;
    int i,j=0;
    NclScalar missing;
    void *arg0,*arg1;
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NrmNameList resources;
	int res_count;
    ng_size_t res_count_size_t;
	static int first = 1;
	static int hlu_class_count = 29;
	static NhlClass NhlPublicClassList[29];
	NhlClass class = NULL;
	int filter = 0;
	regex_t expr;
	regmatch_t rm;

	if (first)  {
		NhlPublicClassList[0] =  NhltickMarkClass;
		NhlPublicClassList[1] =  NhltitleClass;
		NhlPublicClassList[2] =  NhlxWorkstationClass; 
		NhlPublicClassList[3] =  NhlimageWorkstationClass;
		NhlPublicClassList[4] =  NhlncgmWorkstationClass;
		NhlPublicClassList[5] =  NhlcontourPlotClass; 
		NhlPublicClassList[6] =  NhltextItemClass; 
		NhlPublicClassList[7] =  NhlxyPlotClass;
		NhlPublicClassList[8] =  NhllabelBarClass; 
		NhlPublicClassList[9] =  NhllegendClass;
		NhlPublicClassList[10] =  NhlcoordArraysClass; 
		NhlPublicClassList[11] =  NhlscalarFieldClass;
		NhlPublicClassList[12] =  NhlmeshScalarFieldClass;
		NhlPublicClassList[13] =  NhlmapPlotClass;
		NhlPublicClassList[14] =  NhllogLinPlotClass;
		NhlPublicClassList[15] =  NhlirregularPlotClass;
		NhlPublicClassList[16] =  NhlmapPlotClass;
		NhlPublicClassList[17] =  NhlappClass;
		NhlPublicClassList[18] =  NhlannoManagerClass;
		NhlPublicClassList[19] =  NhlpsWorkstationClass;
		NhlPublicClassList[20] =  NhlpdfWorkstationClass;
		NhlPublicClassList[21] =  NhlvectorPlotClass;
		NhlPublicClassList[22] =  NhlvectorFieldClass;
		NhlPublicClassList[23] =  NhlstreamlinePlotClass;
		NhlPublicClassList[24] =  NhlgraphicStyleClass;
		NhlPublicClassList[25] =  NhlprimitiveClass;
		NhlPublicClassList[26] =  NhlcairoDocumentWorkstationClass;
		NhlPublicClassList[27] =  NhlcairoImageWorkstationClass;
		NhlPublicClassList[28] =  NhlcairoWindowWorkstationClass;
	}

        arg0  = NclGetArgValue(
                        0,
                        nargs,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

	if (type == NCL_string) {
		char *name = NrmQuarkToString(*(NrmQuark*)arg0);
		for (i = 0; i < hlu_class_count; i++) {
			size_t len = MIN(strlen(name),strlen(NhlPublicClassList[i]->base_class.class_name));
			if (strncmp(name,NhlPublicClassList[i]->base_class.class_name,len))
				continue;
			class = NhlPublicClassList[i];
			break;
		}
	}
	if (! class) {
		ng_size_t count = 1;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetClassResources: Invalid class name");
		ret = NclReturnValue(
			(void*)&(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
			1,
			&count,
			(void*) &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
			NCL_string,
			1
			);
		return(MIN(ret,NhlWARNING));
	}	
        arg1  = NclGetArgValue(
                        1,
                        nargs,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
	if (type == NCL_string) {
		char *reg_exp = NrmQuarkToString(*(NrmQuark*)arg1);
		if (strlen(reg_exp) > 0) {
			if (regcomp(&expr,reg_exp,REG_ICASE|REG_EXTENDED) != 0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"NhlGetClassResources: Invalid filter expression");
				subret = NhlWARNING;
			}
			else {
				filter = 1;
			}
		}
	}

        resources = (NrmNameList) _NhlGetUserResources(class,&res_count);
        for (i = 0; i < res_count; i++) {
		char *name = NrmQuarkToString(resources[i]);
                if (name[0] == '.')
			continue;
		for (j = i; j < res_count; j++) {
			resources[j - i] = resources[j];
		}
		res_count -= i;
		break;
	}
	if (filter) {
		int new_res_count = 0;
		NrmNameList new_resources;
		for (i = 0; i < res_count; i++) {
			char *res = NrmQuarkToString(resources[i]);
			if (regexec(&expr,res,1,&rm,0) == 0) {
				new_res_count++;
				continue;
			}
			else {
				resources[i] = NrmNULLQUARK;
			}
		}
		if (new_res_count == 0) {
			ng_size_t count = 1;
			ret = NclReturnValue(
				(void*)&(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
				1,
				&count,
				(void*) &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
				NCL_string,
				1
				);
			return(MIN(ret,subret));
				
		}
		else {
			new_resources = NclMalloc(sizeof(NrmQuark) * new_res_count);
			for (i = 0, j = 0; i < new_res_count; i++) {
				while (resources[j] == NrmNULLQUARK)
					j++;
				new_resources[i] = resources[j];
				j++;
			}
			NclFree(resources);
			resources = new_resources;
			res_count = new_res_count;
		}
	}	
	res_count_size_t = res_count;
	ret = NclReturnValue(
                (void*)resources,
                1,
                &res_count_size_t,
                has_missing? &missing : &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
		NCL_string,
                0
        );
	return(ret);
}


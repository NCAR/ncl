#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/DataComm.h>
#include "defs.h"
#include "Symbol.h"

#include "NclDataDefs.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "NclBuiltInSupport.h"
#include "Machine.h"

NhlErrorTypes _NclIChangeWorkstation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing_wk;
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing_wk;
	obj *ncl_hlu_obj_ids;
	obj *wk_obj_id;
	NclHLUObj wk_ptr;
	
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIChangeWorkstation: workstation parameter is missing values");
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
		if(tmp_hlu_ptr[i] != NULL) {
			NhlChangeWorkstation(tmp_hlu_ptr[i]->hlu.hlu_id,wk_ptr->hlu.hlu_id);
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclISetColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 5;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int n_dims_c,dimsizes_c[NCL_MAX_DIMENSIONS];
	int n_dims_r,dimsizes_r[NCL_MAX_DIMENSIONS];
	int n_dims_g,dimsizes_g[NCL_MAX_DIMENSIONS];
	int n_dims_b,dimsizes_b[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type_c;
	NclBasicDataTypes type_r;
	NclBasicDataTypes type_g;
	NclBasicDataTypes type_b;
        int total=1;
        int total_c =1;
        int i,j=0,k;
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
			if((has_missing_c)&&(color_inds[i] != missing_c.intval)) {
				for(k = 0; k < total_c; k++) {
					if( ((!has_missing_r)||(red[i] != missing_r.floatval))&&
					((!has_missing_g)||(green[i] != missing_g.floatval))&&
					((!has_missing_b)||(blue[i] != missing_b.floatval))) {
						NhlSetColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k],red[k],green[k],blue[k]);
					}
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIChangeWorkstation: one of the elements of the workstation parameter does not exist as an HLU workstation, ingnoring");
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclINewColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 4;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int n_dims_c,dimsizes_c[NCL_MAX_DIMENSIONS];
	int n_dims_r,dimsizes_r[NCL_MAX_DIMENSIONS];
	int n_dims_g,dimsizes_g[NCL_MAX_DIMENSIONS];
	int n_dims_b,dimsizes_b[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type_c;
	NclBasicDataTypes type_r;
	NclBasicDataTypes type_b;
	NclBasicDataTypes type_g;
        int total=1;
        int total_c =1;
	int m;
        int i,j=0,k;
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIChangeWorkstation: one of the elements of the workstation parameter does not exist as an HLU workstation, ingnoring");
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
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int total_c =1;
        int i,j=0,k;
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

	for( i = 0; i < j; i++) {
		
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			for(k = 0; k < total_c; k++) {
				if((!has_missing_c)||(color_inds[k] != missing_c.intval)) {
					NhlFreeColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k]);
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIChangeWorkstation: one of the elements of the workstation parameter does not exist as an HLU workstation, ingnoring");
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIIsAllocatedColor
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int total_c =1;
        int i,j=0,k;
	logical *log_out;
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

	log_out = (logical*)NclMalloc(sizeof(logical)*j*total_c);
	for( i = 0; i < j; i++) {
		
		if((tmp_hlu_ptr[i]!= NULL)&&(_NhlIsWorkstation(_NhlGetLayer(tmp_hlu_ptr[i]->hlu.hlu_id)))) {
			for(k = 0; k < total_c; k++) {
				if((!has_missing_c)||(color_inds[k] != missing_c.intval)) {
					log_out[i*j+k] = NhlIsAllocatedColor(tmp_hlu_ptr[i]->hlu.hlu_id,color_inds[k]);
				} else {
					log_out[i*j+k] = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
				}
			} 
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIChangeWorkstation: one of the elements of the workstation parameter does not exist as an HLU workstation, ingnoring");
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
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
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
	out_val = (float*)NclMalloc(sizeof(float)*j*4);
	for( i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			NhlGetBB(tmp_hlu_ptr[i]->hlu.hlu_id,&the_box);
			out_val[i*j] = the_box.t;
			out_val[i*j+1] = the_box.b;
			out_val[i*j+2] = the_box.l;
			out_val[i*j+3] = the_box.r;
		} else {
			out_val[i*j] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*j+1] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*j+2] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
			out_val[i*j+3] = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		}
	}
	if(j == 1) {
		n_dims = 1;
		dimsizes[0] = 4;
	} else {
		n_dims = 2;
		dimsizes[0] = j;
		dimsizes[1] = 4;
	}
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
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,has_missing2;
	int n_dims2,dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type2;
        int total=1;
        int total2=1;
        int i,j=0,k=0,l;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_data_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	string *resname;
	obj *ncl_data_obj_ids;
	int n_dims_ =1, len_dims = 1;
	struct _NclHLUObjRec *tmp_hlu;
	NclStackEntry data_out;
	int tmp;
	int *newid = NclMalloc(sizeof(int));
	NhlLayer tmp_layer;
	
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
	tmp_data_ptr  = (NclHLUObj*)NclMalloc(dimsizes2[0]*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total2; i++) {
			if(ncl_data_obj_ids[i] != missing2.objval) {
				tmp_data_ptr[k] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
				k++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_data_ptr[i] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
		}
		k = total2;
	}
	for(i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			for(l = 0; l < k ; l++) {
				if(tmp_data_ptr[l] != NULL) {
					tmp = NhlAddData(tmp_hlu_ptr[i]->hlu.hlu_id,NrmQuarkToString(*resname),tmp_data_ptr[l]->hlu.hlu_id);
					tmp_layer = _NhlGetLayer(tmp);

					if(tmp > 0) {
                                                tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
                                                *newid =  tmp_hlu->obj.id;
                                                data_out.kind = NclStk_VAL;
                                                data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
                NULL,NULL, Ncl_MultiDValHLUObjData,
                0,(void*)newid,NULL,n_dims_,
                &len_dims,TEMPORARY,NULL);

                                                _NclPlaceReturn(data_out);
                                                return(NhlNOERROR);


                                        } else {
                                                NclFree(newid);
                                                data_out.kind = NclStk_NOVAL;
                                                data_out.u.data_obj= NULL;
                                                _NclPlaceReturn(data_out);
                                                return(NhlFATAL);
                                        }

				}
			}
		}
	}
}
NhlErrorTypes _NclIRemoveData
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,has_missing2;
	int n_dims2,dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
	NclBasicDataTypes type2;
        int total=1;
        int total2=1;
        int i,j=0,k=0,l;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *tmp_data_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	string *resname;
	obj *ncl_data_obj_ids;
	
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
	tmp_data_ptr  = (NclHLUObj*)NclMalloc(total*sizeof(NclHLUObj));
	if(has_missing) {
		for( i = 0; i < total2; i++) {
			if(ncl_data_obj_ids[i] != missing2.objval) {
				tmp_data_ptr[k] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
				k++;
			}
		}
	} else {
		for( i = 0; i < total; i++) {
			tmp_data_ptr[i] = (NclHLUObj)_NclGetObj(ncl_data_obj_ids[i]);
		}
		k = total2;
	}
	for(i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			for(l = 0; l < k ; l++) {
				if(tmp_data_ptr[l] != NULL) {
					NhlRemoveData(tmp_hlu_ptr[i]->hlu.hlu_id,NrmQuarkToString(*resname),tmp_data_ptr[l]->hlu.hlu_id);
				}
			}
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIRemoveOverlay
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing1,n_dims1,dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2;
	NclBasicDataTypes type;
	NclBasicDataTypes type1;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj overlay_obj_ptr;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	obj *ncl_plot_obj_ids;
	logical *restore;
	
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
				NhlRemoveOverlay(overlay_obj_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id,(has_missing2 ? ( missing2.logicalval == *restore ? 0 : *restore) : *restore));
			}
		}
	}
}
NhlErrorTypes _NclIAddToOverlay2
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing,n_dims1,dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing1;
	int has_missing2;
	NclBasicDataTypes type1;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj *base_hlu_ptr;
	NclHLUObj *after_hlu_ptr;
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
}
NhlErrorTypes _NclIAddAnnotation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,has_missing1,n_dims1,n_dims,dimsizes[NCL_MAX_DIMENSIONS],dimsizes1[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1,n_dims_ = 1, len_dims = 1;
        int i,j=0,tmp;
	int *newid = NclMalloc(sizeof(int));
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_base_ptr;
	struct _NclHLUObjRec *tmp_hlu;
	NclScalar missing;
	NclScalar missing1;
	obj *ncl_hlu_obj_ids;
	obj *ncl_ano_obj_ids;
	NclStackEntry data_out;
	NhlLayer tmp_layer;
	
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
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_ids)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_ids);
		if(tmp_base_ptr != NULL) {
			for( i = 0; i < j; i++) {
				if(tmp_hlu_ptr[i] != NULL ) { 
					tmp= NhlAddAnnotation(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id);
					tmp_layer = _NhlGetLayer(tmp);

					if(tmp > 0) {
						tmp_hlu = _NclHLUObjCreate(NULL,NULL,Ncl_HLUObj,0,STATIC,tmp,-1,tmp_layer->base.layer_class);
						*newid =  tmp_hlu->obj.id;
						data_out.kind = NclStk_VAL;
						data_out.u.data_obj = _NclMultiDValHLUObjDataCreate(
                NULL,NULL, Ncl_MultiDValHLUObjData,
                0,(void*)newid,NULL,n_dims_,
                &len_dims,TEMPORARY,NULL);

						_NclPlaceReturn(data_out);
						return(NhlNOERROR);
						

					} else {
						NclFree(newid);
						data_out.kind = NclStk_NOVAL;
						data_out.u.data_obj= NULL;
						_NclPlaceReturn(data_out);
						return(NhlFATAL);
					}
				}
			}
		}
	} else {
		return(NhlFATAL);
	}
}
NhlErrorTypes _NclIRemoveAnnotation
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 2;
	int has_missing,has_missing1,n_dims1,n_dims,dimsizes1[NCL_MAX_DIMENSIONS],dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclHLUObj tmp_base_ptr;
	NclScalar missing;
	NclScalar missing1;
	obj *ncl_hlu_obj_ids;
	obj *ncl_ano_obj_ids;
	
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
			tmp_hlu_ptr[i] = (NclHLUObj)_NclGetObj(ncl_hlu_obj_ids[i]);
		}
		j = total;
	}
	if((!has_missing)||(missing.objval != *ncl_hlu_obj_ids)) {
		tmp_base_ptr = (NclHLUObj)_NclGetObj(*ncl_hlu_obj_ids);
		if(tmp_base_ptr != NULL) {
			for( i = 0; i < j; i++) {
				if(tmp_hlu_ptr[i] != NULL ) { 
					NhlRemoveAnnotation(tmp_base_ptr->hlu.hlu_id,tmp_hlu_ptr[i]->hlu.hlu_id);
				}
			}
		}
	} else {
		return(NhlFATAL);
	}
}
NhlErrorTypes _NclIUpdateData
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	obj *ncl_hlu_obj_ids;
	
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
			NhlUpdateData(tmp_hlu_ptr[i]->hlu.hlu_id);
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIDataPolyline 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,n_dims1,dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2,n_dims2,dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	float *x;
	float *y;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	x = (float*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
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
	if(dimsizes1[0] != dimsizes2[0]) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: x and y parameters must have the same dimension size");
		return(NhlFATAL);
	}
	if(has_missing1){
		for( i = 0; i < n_dims1; i++) {
			if(x[i] == missing1.floatval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
			}
		}
	}
	if(has_missing2){
		for( i = 0; i < n_dims2; i++) {
			if(y[i] == missing2.floatval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
			}
		}
	}
	for( i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			NhlDataPolyline(tmp_hlu_ptr[i]->hlu.hlu_id,x,y,n_dims1);
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclINDCPolyline 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 3;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	int has_missing1,n_dims1,dimsizes1[NCL_MAX_DIMENSIONS];
	int has_missing2,n_dims2,dimsizes2[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
	NclHLUObj *tmp_hlu_ptr;
	NclScalar missing;
	NclScalar missing1;
	NclScalar missing2;
	obj *ncl_hlu_obj_ids;
	float *x;
	float *y;
	
	ncl_hlu_obj_ids = (obj*)NclGetArgValue(
			0,
			nargs,
			&n_dims,
			dimsizes,
			&missing,
			&has_missing,
			&type,
			0);

	x = (float*)NclGetArgValue(
			1,
			nargs,
			&n_dims1,
			dimsizes1,
			&missing1,
			&has_missing1,
			NULL,
			0);

	y = (float*)NclGetArgValue(
			2,
			nargs,
			&n_dims2,
			dimsizes2,
			&missing2,
			&has_missing2,
			NULL,
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
	if(dimsizes1[0] != dimsizes2[0]) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: x and y parameters must have the same dimension size");
		return(NhlFATAL);
	}
	if(has_missing1){
		for( i = 0; i < n_dims1; i++) {
			if(x[i] == missing1.floatval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
			}
		}
	}
	if(has_missing2){
		for( i = 0; i < n_dims2; i++) {
			if(y[i] == missing2.floatval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"DataPolyLine: missing value detected,  x and y parameters must not contain any missing values");
			}
		}
	}
	for( i = 0; i < j; i++) {
		if(tmp_hlu_ptr[i] != NULL) {
			NhlNDCPolyline(tmp_hlu_ptr[i]->hlu.hlu_id,x,y,n_dims1);
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIClassName 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int nargs = 1;
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
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
	int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type;
        int total=1;
        int i,j=0;
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
	return(NclReturnValue(
                (void*)outpt,
                1,
                dimsizes,
                &(((NclTypeClass)nclTypestringClass)->type_class.default_mis),
		NCL_string,
                0
        ));
}

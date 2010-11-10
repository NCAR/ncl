#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(vinth2p,VINTH2P)(double *dati, double *dato, 
				     double *hbcofa, double *hbcofb, 
				     double *p0, double *plevi, 
				     double *plevo, int *intyp, int *ilev, 
				     double *psfc, double *spvl, int *kxtrp,
				     int *imax, int *nlat, int *nlevi, 
				     int *nlevip1, int *nlevo);

NhlErrorTypes vinth2p_W
#if	NhlNeedProto
(void)
#else
()
#endif
{
	ng_size_t i,j;
        ng_size_t sz = 0;
        ng_size_t nblk = 0;
        ng_size_t nblk_out,psf_blk;
	NclVar	tmp_var;
	NclVar lev_coord_var;
	NclMultiDValData lev_coord_md;
	int ids[5];
	NclDimRec dim_info[5];

	NclStackEntry data,val,plevo_val;
	NclMultiDValData tmp_md = NULL;
        NclMultiDValData datai_md;
	char *datai = NULL;
	char *datao;
	ng_size_t datao_dimsizes[5];
	int datai_n_dims,datai_has_missing;
	NclBasicDataTypes datai_type;
	ng_size_t datai_dimsizes[5];
	NclScalar datai_missing;
	NclQuark plevo_quark = 0;
	double *tmp_datao;
	double *tmp_datai;


	double *hbcofa = NULL;
	void *hbcofa_ptr = NULL;
	NclBasicDataTypes hbcofa_type;
	ng_size_t hbcofa_dimsizes;

	double *hbcofb = NULL;
	void *hbcofb_ptr = NULL;
	NclBasicDataTypes hbcofb_type;
	ng_size_t hbcofb_dimsizes;

	char *plevo = NULL;
	char *plevo2 = NULL;
	NclBasicDataTypes plevo_type = NCL_none;
	ng_size_t plevo_dimsizes;
	int plevo_was_val = 0;
	

	int *intyp = NULL;

	void *psfc = NULL;
	double *psfc_d = NULL;
	int psfc_n_dims,psfc_has_missing;
	NclBasicDataTypes psfc_type;
	ng_size_t psfc_dimsizes[4];
	NclScalar psfc_missing;

	void *p0_ptr = NULL;
	double *p0 = NULL;
	NclBasicDataTypes p0_type;

	int *ilev = NULL;
        ng_size_t nlevi, nlevip1;
	ng_size_t total;

	logical* kxtrp = NULL;

	double *plevi;
	NclScalar missing;
	NclScalar out_missing;
	int was_val = 0;
	int not_double = 0;
	int psf_elem;
	NclTypeClass plevo_type_class = NCL_none;
	int idsz0, idsz1, idsz2, iplev, inlevi, inlevip1;

/*
 * Get the first argument. This will be the one that determines
 * partial information about the return array, including number of
 * dimensions, whether or not coordinate arrays are returned, and some
 * return array sizes. 
 */
        val = _NclGetArg(0,9,DONT_CARE);
/*
 * Should be constrained to be a SCALAR md. (What does this mean, anyway?)
 */
        switch(val.kind) {
        case NclStk_VAL:
/* 
 * If NclStk_VAL, then this means that the data variable coming in 
 * has no coordinate information attached to it. This is possibly
 * because in the call to vinth2p, an arithmetic expression was used
 * for the input rather than just a variable.
 */
		was_val = 1;
                datai_md= val.u.data_obj;
                break;
        case NclStk_VAR:
/*
 * If NclStk_VAR, then this means that the data variable coming in 
 * probably has coordinate information attached to it. 
 */
                datai_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
                break;
        default:
                return(NhlFATAL);
        }
	datai_type = datai_md->multidval.data_type;
	datai_n_dims = datai_md->multidval.n_dims;
	if( datai_n_dims < 3  || datai_n_dims > 5 ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: requires a minimum of 3 dimensions [lev]x[lat]x[lon] and a maximum of 5 dimensions [case]x[time]x[lev]x[lat]x[lon], %d dimensions passed in",datai_n_dims);
	  return(NhlFATAL);
	} else {
/*
 * 'total' is the size of the leftmost dimensions (minus 
 * the 3 rightmost dimensions).
 */
	  if(datai_n_dims == 3)  {
	    total = 1;
	  } else if(datai_n_dims == 4)  {
	    total = datai_md->multidval.dim_sizes[0];
	  }
	  else {        /* datai_n_dims better be 5 */ 
	    total = datai_md->multidval.dim_sizes[0] *
	      datai_md->multidval.dim_sizes[1];
	  }
	}
/*
 * The Fortran array is expecting a lev x lat x lon array, so store
 * that information here.
 */
	datai_dimsizes[0] = datai_md->multidval.dim_sizes[datai_n_dims - 3];
	datai_dimsizes[1] = datai_md->multidval.dim_sizes[datai_n_dims - 2];
	datai_dimsizes[2] = datai_md->multidval.dim_sizes[datai_n_dims - 1];
	datai_has_missing = datai_md->multidval.missing_value.has_missing;
	datai_missing = datai_md->multidval.missing_value.value;
/*
 * Determine if the input is double, or anything else, and calculate
 * its size.
 */
	if(datai_md != NULL) {
		switch(datai_type) {
		case NCL_double:
			sz = sizeof(double);
			break;
		default:
			sz = datai_md->multidval.type->type_class.size;
			not_double = 1;
			break;
		}
		datai = (char*)datai_md->multidval.val;
		nblk = datai_md->multidval.dim_sizes[datai_n_dims - 1] * datai_md->multidval.dim_sizes[datai_n_dims - 2] * datai_md->multidval.dim_sizes[datai_n_dims - 3];
	}
/*
 * Get some of the other arguments.
 */
        hbcofa_ptr = (void*)NclGetArgValue(
                        1,
                        9,
                        NULL,
                        &hbcofa_dimsizes,
                        NULL,
                        NULL,
                        &hbcofa_type,
                        0);
	if(hbcofa_type != NCL_double ) {
		hbcofa = (double*)NclMalloc(sizeof(double) * hbcofa_dimsizes);
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)hbcofa,(void*)hbcofa_ptr,hbcofa_dimsizes,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(hbcofa_type))));
	} else {
		hbcofa = (double*) hbcofa_ptr;
	}
        hbcofb_ptr = (void*)NclGetArgValue(
                        2,
                        9,
                        NULL,
                        &hbcofb_dimsizes,
                        NULL,
                        NULL,
                        &hbcofb_type,
                        0);
	if(hbcofb_type != NCL_double ) {
		hbcofb = (double*)NclMalloc(sizeof(double) * hbcofb_dimsizes);
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)hbcofb,(void*)hbcofb_ptr,hbcofb_dimsizes,NULL,NULL,
(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(hbcofb_type))));
	} else {
		hbcofb = (double*) hbcofb_ptr;
	}
/*
 * The length of plevo, which is a 1D array, determines the length
 * of the level dimension of the return array.
 */
        plevo_val = _NclGetArg(3,9,DONT_CARE);
	switch(plevo_val.kind) {
	case NclStk_VAL:
		plevo_was_val = 1;
		plevo_dimsizes = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.dim_sizes[0];
		plevo_type = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.data_type;
		plevo_quark = NrmStringToQuark("lev_p");
		plevo_type_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type)));
		if(plevo_type != NCL_double ) {
			plevo = (char*)NclMalloc(sizeof(double) * plevo_dimsizes);
			_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)plevo,((NclMultiDValData)(plevo_val.u.data_obj))->multidval.val,plevo_dimsizes,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type))));
		} else {
			plevo = (char*)((NclMultiDValData)(plevo_val.u.data_obj))->multidval.val;
		}
		tmp_md = plevo_val.u.data_obj;
		break;
	case NclStk_VAR:
		plevo_was_val = 0;
                tmp_md = _NclVarValueRead(plevo_val.u.data_var,NULL,NULL);
		plevo_dimsizes = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_size;
		plevo_type = tmp_md->multidval.data_type;
		plevo_quark = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_quark;
		if(plevo_quark == -1) 
			plevo_quark = NrmStringToQuark("lev_p");
		plevo_type_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type)));
		if(plevo_type != NCL_double ) {
			plevo = (char*)NclMalloc(sizeof(double) * plevo_dimsizes);
			_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)plevo,tmp_md->multidval.val,plevo_dimsizes,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type))));
		} else {
			plevo = (char*)tmp_md->multidval.val;
		}
		break;
	default:
		break;
	}
	if(plevo_type != NCL_double) {
		plevo2 = (char*)NclMalloc(tmp_md->multidval.totalsize);
		memcpy(plevo2,tmp_md->multidval.val,tmp_md->multidval.totalsize);
	} else {
		plevo2 = (char*)NclMalloc(tmp_md->multidval.totalsize);
		memcpy(plevo2,tmp_md->multidval.val,tmp_md->multidval.totalsize);
	}
	nblk_out = plevo_dimsizes * datai_md->multidval.dim_sizes[ datai_md->multidval.n_dims -1] * datai_md->multidval.dim_sizes[ datai_md->multidval.n_dims -2];
	
/*
 * psfc must the same size as the first input argument, minus the level
 * dimemsion.
 */
        psfc = (void*)NclGetArgValue(
                        4,
                        9,
                        &psfc_n_dims,
                        psfc_dimsizes,
                        &psfc_missing,
                        &psfc_has_missing,
                        &psfc_type,
                        0);
	if(psfc_has_missing) {
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)&missing,(void*)&psfc_missing,1,NULL,NULL, (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
		out_missing = psfc_missing;
	} else if(datai_has_missing) {
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)&missing,(void*)&datai_missing,1,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
		out_missing = datai_missing;
	} else {
		missing = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
		if(not_double) {
			out_missing.floatval = (float)missing.doubleval;
		} else {
			out_missing = missing;
		}
	}
	
	
	if(psfc_n_dims != datai_n_dims -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: Surface pressure must have same number of [case], [time], lat and lon elements as first input argument; number of dimensions do not match.");
		return(NhlFATAL);
	} else {
	        if(datai_n_dims == 5) {
		  if(datai_md->multidval.dim_sizes[0] != psfc_dimsizes[0] ||
		     datai_md->multidval.dim_sizes[1] != psfc_dimsizes[1]) {
		    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: Surface pressure must have same number of case and time elements as input.");
		    return(NhlFATAL);
		  }
		}
		else if(datai_n_dims == 4) {
		  if(datai_md->multidval.dim_sizes[0] != psfc_dimsizes[0]) {
		    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: Surface pressure must have same number of time elements as input.");
		    return(NhlFATAL);
		  }
		}
		if(datai_md->multidval.dim_sizes[datai_md->multidval.n_dims - 2] != psfc_dimsizes[psfc_n_dims - 2]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: Surface pressure must have same number of latitude elements as input.");
			return(NhlFATAL);
		}
		if(datai_md->multidval.dim_sizes[datai_md->multidval.n_dims - 1] != psfc_dimsizes[psfc_n_dims - 1]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: Surface pressure must have same number of longitude elements as input.");
			return(NhlFATAL);
		}
	}
	psf_blk = psfc_dimsizes[psfc_n_dims -1] * psfc_dimsizes[psfc_n_dims -2];
	psf_elem = 1;
	for(i = 0; i < psfc_n_dims; i++) {
		psf_elem *= psfc_dimsizes[i];
	}
	
/*
 * Get the rest of the arguments.
 */
        intyp = (int*)NclGetArgValue(
                        5,
                        9,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
        p0_ptr = (void*)NclGetArgValue(
                        6,
                        9,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        &p0_type,
                        0);
	if(p0_type != NCL_double ) {
		p0  = (double*)NclMalloc(sizeof(double));
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)p0,(void*)p0_ptr,1,NULL,NULL,
(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(p0_type))));
	} else {
		p0 = (double*) p0_ptr;
	}
        ilev = (int*)NclGetArgValue(
                        7,
                        9,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
        kxtrp = (logical*)NclGetArgValue(
                        8,
                        9,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

/*
 * Calculate size for plevi that will be passed to Fortran routine.
 */
	nlevi   = datai_dimsizes[0];
	nlevip1 = nlevi+1;
	plevi   = (double*)NclMalloc(nlevip1*sizeof(double));
/*
 * Test dimension sizes.
 */
	if((datai_dimsizes[0] > INT_MAX) || (datai_dimsizes[1] > INT_MAX) ||
	   (datai_dimsizes[2] > INT_MAX) || (plevo_dimsizes > INT_MAX) ||
	   (nlevi > INT_MAX) ||(nlevip1 > INT_MAX)) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p: one or more dimension sizes is greater than INT_MAX");
	  return(NhlFATAL);
	}
	idsz0 = (int) datai_dimsizes[0];
	idsz1 = (int) datai_dimsizes[1];
	idsz2 = (int) datai_dimsizes[2];
	iplev = (int) plevo_dimsizes;
	inlevi = (int) nlevi;
	inlevip1 = (int) nlevip1;

	if(not_double) {
/*
 * Create space for datao array, and temporary input/output arrays
 * that will be passed to the Fortran routine.
 */
		datao = (char*)NclMalloc(total * nblk_out * sizeof(float));
		tmp_datai = (double*)NclMalloc(nblk * sizeof(double));
		tmp_datao = (double*)NclMalloc(nblk_out* sizeof(double));
		psfc_d = (double*) NclMalloc(psf_blk*sizeof(double));
		
/*
 * Loop across leftmost dimensions and pass subsections of the input
 * arrays to the Fortran routine.
 */
		for(i = 0; i < total ; i++) {
/*
 * The input is not double, so we have to coerce it.
 */
			_Nclcoerce((NclTypeClass)nclTypedoubleClass, tmp_datai, (datai+i*sz*nblk),nblk,NULL,NULL,datai_md->multidval.type);
			_Nclcoerce((NclTypeClass)nclTypedoubleClass, psfc_d, ((char*)psfc+sz*psf_blk*i),psf_blk,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));

/*
 * Here's the call to the Fortran routine.
 */
			NGCALLF(vinth2p,VINTH2P)(tmp_datai,tmp_datao,hbcofa,hbcofb,
						 p0,plevi,(double *)plevo,intyp,ilev,
						 psfc_d,(double *)(&missing),kxtrp,
						 &idsz2,&idsz1,&inlevi,&inlevip1,&iplev);

/*
 * Copy the output values back to the float array. 
 */
			for(j = 0; j< nblk_out; j++) {
				((float*)datao)[i*nblk_out + j] = (float)tmp_datao[j];
			}
		}
		NclFree(psfc_d);
		NclFree(tmp_datai);
		NclFree(tmp_datao);
		
	} else {
/*
 * The input is already double, which makes the code a little simpler here.
 *
 * Create space for datao return array.
 */
		datao = (char*)NclMalloc(total * nblk_out * sizeof(double));
		if(psfc_type != NCL_double) {
			psfc_d = (double*) NclMalloc(psf_elem*sizeof(double));
			_Nclcoerce((NclTypeClass)nclTypedoubleClass, psfc_d, (char*)psfc,psf_elem,NULL,NULL,(NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
		} else {
			psfc_d =(double*) psfc;
		}
		for(i = 0; i < total; i++) {
/*
 * Here's the call to the Fortran routine.
 */
		  NGCALLF(vinth2p,VINTH2P)((double *)(datai+sizeof(double)*i*nblk),
					   (double *)(((char*)datao)+sizeof(double)*nblk_out*i),
					   hbcofa,hbcofb,p0,plevi,(double *)plevo,intyp,ilev,
					   (double *)(((char*)psfc_d)+sizeof(double)*psf_blk*i),
					   (double *)(&missing),kxtrp,&idsz2,
					   &idsz1,&idsz0,&idsz0,&iplev);
		  
		}
		if((void*)psfc_d != psfc) {
		  NclFree(psfc_d);
		}
	}

/*
 * Free stuff up.
 */
	NclFree(plevi);


	if(tmp_md->multidval.val != plevo) {
		NclFree(plevo);
	}
	if((void*)hbcofa != hbcofa_ptr) {
		NclFree(hbcofa);
	}
	if((void*)hbcofb != hbcofb_ptr) {
		NclFree(hbcofb);
	}
	if((void*)p0 != p0_ptr) {
		NclFree(p0);
	}
	if(was_val) {
/*
 * If the original input array didn't have coordinate information,
 * then we have to define it here. 
 *
 * There are three different cases here, depending on whether the
 * input was 3D, 4D or 5D.
 *
 * Start with the 5D case.
 */
		if(datai_md->multidval.n_dims == 5 ) {
		  dim_info[0].dim_num   = 0;
		  dim_info[0].dim_quark = NrmStringToQuark("case"); 
		  dim_info[0].dim_size  = datai_md->multidval.dim_sizes[0];

		  dim_info[1].dim_num   = 1;
		  dim_info[1].dim_quark = NrmStringToQuark("time"); 
		  dim_info[1].dim_size  = datai_md->multidval.dim_sizes[1];
		  
		  dim_info[2].dim_num   = 2;
		  dim_info[2].dim_quark = plevo_quark; 
		  dim_info[2].dim_size  = plevo_dimsizes;
		  
		  dim_info[3].dim_num   = 3;
		  dim_info[3].dim_quark = NrmStringToQuark("lat");
		  dim_info[3].dim_size  = datai_dimsizes[1];
		  
		  dim_info[4].dim_num   = 4;
		  dim_info[4].dim_quark = NrmStringToQuark("lon");
		  dim_info[4].dim_size  = datai_dimsizes[2];
		  
		  lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
		  lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[2]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
		  if(!plevo_was_val) {
		    _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
		  }
		  ids[0] = -1;
		  ids[1] = -1;
		  ids[2] = lev_coord_var->obj.id;
		  ids[3] = -1;
		  ids[4] = -1;
		  datao_dimsizes[0] = datai_md->multidval.dim_sizes[0];
		  datao_dimsizes[1] = datai_md->multidval.dim_sizes[1];
		  datao_dimsizes[2] = plevo_dimsizes;
		  datao_dimsizes[3] = datai_md->multidval.dim_sizes[3];
		  datao_dimsizes[4] = datai_md->multidval.dim_sizes[4];

		  tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,5,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
		}
		else if(datai_md->multidval.n_dims == 4 ) {
/*
 * Here's the 4D case.
 */
			dim_info[0].dim_quark =NrmStringToQuark("time"); 
			dim_info[0].dim_num= 0; 
			dim_info[0].dim_size = total; 
			dim_info[1].dim_quark =plevo_quark; 
			dim_info[1].dim_num= 1; 
			dim_info[1].dim_size = plevo_dimsizes; 
			dim_info[2].dim_quark = NrmStringToQuark("lat");
			dim_info[2].dim_size = datai_dimsizes[1]; 
			dim_info[2].dim_num= 2; 
			dim_info[3].dim_quark = NrmStringToQuark("lon");
			dim_info[3].dim_size = datai_dimsizes[2];
			dim_info[3].dim_num= 3; 

			lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
			lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[1]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
			if(!plevo_was_val) {
				_NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
			}
			ids[0] = -1;
			ids[1] = lev_coord_var->obj.id;
			ids[2] = -1;
			ids[3] = -1;
			datao_dimsizes[0] = total;
			datao_dimsizes[1] = plevo_dimsizes;
			datao_dimsizes[2] = datai_dimsizes[1];
			datao_dimsizes[3] = datai_dimsizes[2];
			tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,4,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
		} else {
/*
 * Here's the 3D case.
 */
			dim_info[0].dim_quark =plevo_quark; 
			dim_info[0].dim_num= 0; 
			dim_info[0].dim_size = plevo_dimsizes; 
			dim_info[1].dim_quark = NrmStringToQuark("lat");
			dim_info[1].dim_size = datai_dimsizes[1]; 
			dim_info[1].dim_num= 1; 
			dim_info[2].dim_quark = NrmStringToQuark("lon");
			dim_info[2].dim_size = datai_dimsizes[2];
			dim_info[2].dim_num= 2; 
			lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
			lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,dim_info,-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
			if(!plevo_was_val) {
				_NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
			}
			ids[0] = lev_coord_var->obj.id;
			ids[1] = -1;
			ids[2] = -1;
			datao_dimsizes[0] = plevo_dimsizes;
			datao_dimsizes[1] = datai_dimsizes[1];
			datao_dimsizes[2] = datai_dimsizes[2];
			tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,3,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
			
		}
		data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);
		data.kind = NclStk_VAR;
		_NclPlaceReturn(data);
	} else {
/*
 * The input data may or may not have coordinate information attached
 * to it.
 *
 * There are three different cases here, depending on whether the
 * input was 3D, 4D or 5D.
 *
 * Start with the 5D case.
 */
		if(datai_n_dims == 5) {
		  dim_info[0].dim_num   = 0; 
		  dim_info[0].dim_quark = val.u.data_var->var.dim_info[0].dim_quark;
		  dim_info[0].dim_size  = val.u.data_var->var.dim_info[0].dim_size;
		  
		  dim_info[1].dim_num   = 1;
		  dim_info[1].dim_quark = val.u.data_var->var.dim_info[1].dim_quark;
		  dim_info[1].dim_size  = val.u.data_var->var.dim_info[1].dim_size;
		  
		  dim_info[2].dim_num   = 2;
		  dim_info[2].dim_quark = plevo_quark;
		  dim_info[2].dim_size  = plevo_dimsizes;
		  
		  dim_info[3].dim_num   = 3;
		  dim_info[3].dim_quark = val.u.data_var->var.dim_info[3].dim_quark;
		  dim_info[3].dim_size  = val.u.data_var->var.dim_info[3].dim_size; 
		  
		  dim_info[4].dim_num   = 4;
		  dim_info[4].dim_quark = val.u.data_var->var.dim_info[4].dim_quark;
		  dim_info[4].dim_size  = val.u.data_var->var.dim_info[4].dim_size; 
		  
		  lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
		  lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[2]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
		  if(!plevo_was_val) {
		    _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
		  }
		  ids[0] = -1;
		  ids[1] = -1;
		  ids[2] = lev_coord_var->obj.id;
		  ids[3] = -1;
		  ids[4] = -1;
		  datao_dimsizes[0] = val.u.data_var->var.dim_info[0].dim_size;
		  datao_dimsizes[1] = val.u.data_var->var.dim_info[1].dim_size;
		  datao_dimsizes[2] = plevo_dimsizes;
		  datao_dimsizes[3] = val.u.data_var->var.dim_info[3].dim_size;
		  datao_dimsizes[4] = val.u.data_var->var.dim_info[4].dim_size;

		  tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,5,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
		  data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);

/*
 * Here's where we check if the input variable had any dimension
 * information attached to it. If so, use it to return it with the
 * return output.
 *
 * Since this is the 5D case, we need to check dimensions 0, 1, 3, and 4.
 * (The 2-th dimension is the level dimension that we've already dealt
 * with above.)
 */
		  if((val.u.data_var->var.dim_info[0].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark)))) {
		    tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark),NULL);
		    _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[0].dim_quark),NULL);
		    if(data.u.data_var->var.coord_vars[0] != -1) {
		      _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[0]),tmp_var);
		    }
		  }
		  
		  if((val.u.data_var->var.dim_info[1].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark)))) {
		    tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
		    _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[1].dim_quark),NULL);
		    if(data.u.data_var->var.coord_vars[1] != -1) {
		      _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[1]),tmp_var);
		    }
		  }
		  
		  if((val.u.data_var->var.dim_info[3].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark)))) {
		    tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark),NULL);
		    _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[3].dim_quark),NULL);
		    if(data.u.data_var->var.coord_vars[3] != -1) {
		      _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[3]),tmp_var);
		    }
		  }
		  if((val.u.data_var->var.dim_info[4].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[4].dim_quark)))) {
		    tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[4].dim_quark),NULL);
		    _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[4].dim_quark),NULL);
		    if(data.u.data_var->var.coord_vars[4] != -1) {
		      _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[4]),tmp_var);
		    }
		  }
		  
		}
		else if(datai_n_dims == 4) {
/*
 * Here's the 4D case.
 */
			dim_info[0].dim_quark = val.u.data_var->var.dim_info[0].dim_quark;
			dim_info[0].dim_num= 0; 
			dim_info[0].dim_size = val.u.data_var->var.dim_info[0].dim_size; 
			dim_info[1].dim_quark = plevo_quark;
			dim_info[1].dim_num= 1; 
			dim_info[1].dim_size = plevo_dimsizes; 
			dim_info[2].dim_quark = val.u.data_var->var.dim_info[2].dim_quark;
			dim_info[2].dim_size = val.u.data_var->var.dim_info[2].dim_size; 
			dim_info[2].dim_num= 2; 
			dim_info[3].dim_quark = val.u.data_var->var.dim_info[3].dim_quark;
			dim_info[3].dim_size = val.u.data_var->var.dim_info[3].dim_size; 
			dim_info[3].dim_num= 3; 
			lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
			lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[1]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
			if(!plevo_was_val) {
				_NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
			}
			ids[0] = -1;
			ids[1] = lev_coord_var->obj.id;
			ids[2] = -1;
			ids[3] = -1;
			datao_dimsizes[0] = total;
			datao_dimsizes[1] = plevo_dimsizes;
			datao_dimsizes[2] = val.u.data_var->var.dim_info[2].dim_size;
			datao_dimsizes[3] = val.u.data_var->var.dim_info[3].dim_size;
			tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,4,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
			data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);

/*
 * Here's where we check if the input variable had any dimension
 * information attached to it. If so, use it to return it with the
 * return output.
 *
 * Since this is the 4D case, we need to check dimensions 0, 2, and 3.
 * (The 1-th dimension is the level dimension that we've already dealt
 * with above.)
 */
			if((val.u.data_var->var.dim_info[0].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark)))) {
				tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark),NULL);
				_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[0].dim_quark),NULL);
				if(data.u.data_var->var.coord_vars[0] != -1) {
					_NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[0]),tmp_var);
				}
			}

			if((val.u.data_var->var.dim_info[2].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark)))) {
				tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark),NULL);
				_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[2].dim_quark),NULL);
				if(data.u.data_var->var.coord_vars[2] != -1) {
					_NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[2]),tmp_var);
				}
			}
	
			if((val.u.data_var->var.dim_info[3].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark)))) {
				tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark),NULL);
				_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[3].dim_quark),NULL);
				if(data.u.data_var->var.coord_vars[3] != -1) {
					_NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[3]),tmp_var);
				}
			}
		} else {
/*
 * Here's the 3D case.
 */
			dim_info[0].dim_quark = plevo_quark;
			dim_info[0].dim_num= 0; 
			dim_info[0].dim_size = plevo_dimsizes; 
			dim_info[1].dim_quark = val.u.data_var->var.dim_info[1].dim_quark;
			dim_info[1].dim_size = val.u.data_var->var.dim_info[1].dim_size; 
			dim_info[1].dim_num= 1; 
			dim_info[2].dim_quark = val.u.data_var->var.dim_info[2].dim_quark;
			dim_info[2].dim_size = val.u.data_var->var.dim_info[2].dim_size; 
			dim_info[2].dim_num= 2; 
			lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)plevo_type_class);
			lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,dim_info,-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
			if(!plevo_was_val) {
				_NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
			}
			ids[0] = lev_coord_var->obj.id;
			ids[1] = -1;
			ids[2] = -1;
			datao_dimsizes[0] = plevo_dimsizes;
			datao_dimsizes[1] = datai_dimsizes[1];
			datao_dimsizes[2] = datai_dimsizes[2];
			tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&out_missing,3,datao_dimsizes,TEMPORARY,NULL,not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
			data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);

/*
 * Here's where we check if the input variable had any dimension
 * information attached to it. If so, use it to return it with the
 * return output.
 *
 * Since this is the 3D case, we need to check dimensions 1 and 2.
 * (The 1-th dimension is the level dimension that we've already dealt
 * with above.)
 */
			if((val.u.data_var->var.dim_info[1].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark)))) {
				tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
				_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
				if(data.u.data_var->var.coord_vars[1] != -1) {
					_NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[1]),tmp_var);
				}
			}
	
			if((val.u.data_var->var.dim_info[2].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark)))) {
				tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark),NULL);
				_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark),NULL);
				if(data.u.data_var->var.coord_vars[2] != -1) {
					_NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[2]),tmp_var);
				}
			}
		}
		data.kind = NclStk_VAR;
		_NclPlaceReturn(data);
	}
	return(NhlNOERROR);
}

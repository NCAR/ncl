#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dvinth2pecmwfnodes,DVINTH2PECMWFNODES)(
                                     double *dati, double *dato, 
                                     double *hbcofa, double *hbcofb, 
                                     double *p0, double *plevi, 
                                     double *plevo, int *intyp, int *ilev, 
                                     double *psfc, double *spvl, int *kxtrp,
                                     int *npts, int *nlevi, int *nlevip1, 
                                     int *nlevo,int *varflg, double *tbot,
                                     double *phis);

NhlErrorTypes vinth2p_ecmwf_nodes_W
#if NhlNeedProto
(void)
#else
()
#endif
{
    ng_size_t i, j, nc, nt;
    ng_size_t sz = 0;
    ng_size_t nblk = 0;
    ng_size_t nblk_out, psf_blk, index_phis;
    NclVar  tmp_var;
    NclVar lev_coord_var;
    NclMultiDValData lev_coord_md;
    int ids[4];
    NclDimRec dim_info[4];

    NclStackEntry data,val,plevo_val;
    NclMultiDValData tmp_md = NULL;
    NclMultiDValData datai_md;
    char *datai = NULL,*datao;
    ng_size_t datao_dimsizes[4];
    int datai_n_dims,datai_has_missing;
    NclBasicDataTypes datai_type;
    NclScalar datai_missing;
    NclQuark plevo_quark = 0;
    double *tmp_datao;
    double *tmp_datai;


    double *hbcofa = NULL;
    void *hbcofa_ptr = NULL;
    int hbcofa_n_dims,hbcofa_has_missing;
    NclBasicDataTypes hbcofa_type;
    ng_size_t hbcofa_dimsizes;
    NclScalar hbcofa_missing;

    double *hbcofb = NULL;
    void *hbcofb_ptr = NULL;
    int hbcofb_n_dims,hbcofb_has_missing;
    NclBasicDataTypes hbcofb_type;
    ng_size_t hbcofb_dimsizes;
    NclScalar hbcofb_missing;

    char *plevo = NULL;
    char *plevo2 = NULL;
    int plevo_n_dims,plevo_has_missing;
    NclBasicDataTypes plevo_type = NCL_none;
    ng_size_t nlevo;
    NclScalar plevo_missing;
    int plevo_was_val = 0;
    

    int *intyp = NULL;
    int intyp_has_missing;
    NclScalar intyp_missing;

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
    int ilev_has_missing;
    NclScalar ilev_missing;
    int total;

    logical* kxtrp = NULL;
    int kxtrp_has_missing;
    NclScalar kxtrp_missing;

    int *varflg = NULL;

    void *tbot = NULL;
    double *tbot_d = NULL;
    int tbot_n_dims;
    NclBasicDataTypes tbot_type;
    ng_size_t tbot_dimsizes[3];

    void *phis = NULL;
    double *phis_d = NULL;
    int phis_n_dims;
    NclBasicDataTypes phis_type;
    ng_size_t phis_dimsizes[3];

    double *plevi;
    NclScalar missing;
    NclScalar out_missing;
    int was_val = 0;
    int not_double = 0;
    int psf_elem, phis_elem;
    NclTypeClass plevo_type_class = NCL_none;
    
    ng_size_t ncase, ntime, nlevi, nodes;  /* The 4 possible dims of datai */
    ng_size_t nlevi_nodes, nlevip1;
    int inodes, inlevi, inlevip1, inlevo;

    val = _NclGetArg(0,12,DONT_CARE);
/*
* Should be constrained to be a SCALAR md
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
      datai_type = datai_md->multidval.data_type;
      datai_n_dims = datai_md->multidval.n_dims;
      if((datai_n_dims < 2)||(datai_n_dims>4)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: requires a minimum of 2 dimensions [lev]x[nodes] and a maximum of 4 dimensions [case]x[time]x[lev]x[nodes], %d dimensions passed in",datai_n_dims);
        return(NhlFATAL);
      } else {
/*
 * 'total' is the size of the leftmost dimensions (minus the 2
 * rightmost dimensions).
 */
        if(datai_n_dims == 3)  {
          ncase = 1;
          ntime = 1;
          nlevi = datai_md->multidval.dim_sizes[0];
          nodes = datai_md->multidval.dim_sizes[1];
        }
        else if(datai_n_dims == 3)  {
          ncase = 1;
          ntime = datai_md->multidval.dim_sizes[0];
          nlevi = datai_md->multidval.dim_sizes[1];
          nodes = datai_md->multidval.dim_sizes[2];
        }
        else {                                  /* datai_n_dims better be 4 */ 
          ncase = datai_md->multidval.dim_sizes[0];
          ntime = datai_md->multidval.dim_sizes[1];
          nlevi = datai_md->multidval.dim_sizes[2];
          nodes = datai_md->multidval.dim_sizes[3];
        }
        total       = ntime * ncase;
        nlevi_nodes = nlevi * nodes;
      }
/*
 * Save the missing value, if any.
 */
      datai_has_missing = datai_md->multidval.missing_value.has_missing;
      datai_missing = datai_md->multidval.missing_value.value;
      break;
    case NclStk_VAR:
/*
 * If NclStk_VAR, then this means that the data variable coming in 
 * probably has coordinate information attached to it. 
 */
      datai_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
      datai_n_dims = datai_md->multidval.n_dims;
      if((datai_n_dims < 2)||(datai_n_dims>4)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: requires a minimum of 2 dimensions [lev]x[nodes] and a maximum of 4 dimensions [case]x[time]x[lev]x[nodes], %d dimensions passed in",datai_n_dims);
        return(NhlFATAL);
      } else {
/*
 * 'total' is the size of the leftmost dimensions (minus the 2
 * rightmost dimensions).
 */
        if(datai_n_dims == 2)  {
          ncase = 1;
          ntime = 1;
          nlevi = datai_md->multidval.dim_sizes[0];
          nodes = datai_md->multidval.dim_sizes[1];
        }
        else if(datai_n_dims == 3)  {
          ncase = 1;
          ntime = datai_md->multidval.dim_sizes[0];
          nlevi = datai_md->multidval.dim_sizes[1];
          nodes = datai_md->multidval.dim_sizes[2];
        }
        else {                                  /* datai_n_dims better be 4 */ 
          ncase = datai_md->multidval.dim_sizes[0];
          ntime = datai_md->multidval.dim_sizes[1];
          nlevi = datai_md->multidval.dim_sizes[2];
          nodes = datai_md->multidval.dim_sizes[3];
        }
        total       = ncase * ntime;
        nlevi_nodes = nlevi * nodes;
      }
/*
 * Save the missing value, if any.
 */
      datai_has_missing = datai_md->multidval.missing_value.has_missing;
      datai_missing = datai_md->multidval.missing_value.value;
      datai_type = datai_md->multidval.data_type;
      break;
    default:
      return(NhlFATAL);
    }
    if(datai_md != NULL) {
      nblk = nlevi_nodes;
      switch(datai_type) {
      case NCL_double:
        datai = (char*)datai_md->multidval.val;
        sz = sizeof(double);
        break;
      default:
        datai = (char*)datai_md->multidval.val;
        sz = datai_md->multidval.type->type_class.size;
        not_double = 1;
        break;
      }
    }
    hbcofa_ptr = (void*)NclGetArgValue(
                        1,
                        12,
                        &hbcofa_n_dims,
                        &hbcofa_dimsizes,
                        &hbcofa_missing,
                        &hbcofa_has_missing,
                        &hbcofa_type,
                        0);
    if(hbcofa_type != NCL_double ) {
      hbcofa = (double*)NclMalloc(sizeof(double) * hbcofa_dimsizes);
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)hbcofa,
                 (void*)hbcofa_ptr,hbcofa_dimsizes,NULL,NULL,
                 (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(hbcofa_type))));
    } else {
      hbcofa = (double*) hbcofa_ptr;
    }
    hbcofb_ptr = (void*)NclGetArgValue(
                        2,
                        12,
                        &hbcofb_n_dims,
                        &hbcofb_dimsizes,
                        &hbcofb_missing,
                        &hbcofb_has_missing,
                        &hbcofb_type,
                        0);
    if(hbcofb_type != NCL_double ) {
      hbcofb = (double*)NclMalloc(sizeof(double) * hbcofb_dimsizes);
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)hbcofb,
                 (void*)hbcofb_ptr,hbcofb_dimsizes,NULL,NULL,
                 (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(hbcofb_type))));
    } else {
      hbcofb = (double*) hbcofb_ptr;
    }
    plevo_val = _NclGetArg(3,12,DONT_CARE);
    switch(plevo_val.kind) {
    case NclStk_VAL:
      plevo_was_val = 1;
      plevo_n_dims = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.n_dims;
      nlevo = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.dim_sizes[0];
      plevo_missing = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.missing_value.value;
      plevo_has_missing = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.missing_value.has_missing;
      plevo_type = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.data_type;
      plevo_quark = NrmStringToQuark("lev_p");
      plevo_type_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type)));
      if(plevo_type != NCL_double ) {
        plevo = (char*)NclMalloc(sizeof(double) * nlevo);
        _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)plevo,
                   ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.val,
                   nlevo,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type))));
      } else {
        plevo = (char*)((NclMultiDValData)(plevo_val.u.data_obj))->multidval.val;
      }
      tmp_md = plevo_val.u.data_obj;
      break;
    case NclStk_VAR:
      plevo_was_val = 0;
      tmp_md = _NclVarValueRead(plevo_val.u.data_var,NULL,NULL);
      plevo_n_dims = ((NclVarRec*)(plevo_val.u.data_var))->var.n_dims;
      nlevo = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_size;
      plevo_missing = tmp_md->multidval.missing_value.value;
      plevo_has_missing = tmp_md->multidval.missing_value.has_missing;
      plevo_type = tmp_md->multidval.data_type;
      plevo_quark = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_quark;
      if(plevo_quark == -1) {
        plevo_quark = NrmStringToQuark("lev_p");
      }
      plevo_type_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type)));
      if(plevo_type != NCL_double ) {
        plevo = (char*)NclMalloc(sizeof(double) * nlevo);
        _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)plevo,
                   tmp_md->multidval.val,nlevo,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(plevo_type))));
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
    nblk_out = nlevo * nodes;
    
    psfc = (void*)NclGetArgValue(
                        4,
                        12,
                        &psfc_n_dims,
                        psfc_dimsizes,
                        &psfc_missing,
                        &psfc_has_missing,
                        &psfc_type,
                        0);
    if(psfc_has_missing) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 (void*)&missing,(void*)&psfc_missing,1,NULL,NULL,
                 (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
      out_missing = psfc_missing;
    } else if(datai_has_missing) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)&missing,
                 (void*)&datai_missing,1,NULL,NULL,
                 (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
      out_missing = datai_missing;
    } else {
      missing = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
      if(not_double) {
        out_missing.floatval = (float)missing.doubleval;
      } else {
        out_missing= missing;
      }
    }
    
    if(psfc_n_dims != datai_n_dims -1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface pressure must have same number of case, time, and nodes elements as input, number of dimensions does not match.");
      return(NhlFATAL);
    }
    else {
      if(datai_n_dims == 4) {
        if(psfc_dimsizes[0] != ncase || psfc_dimsizes[1] != ntime) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface pressure must have same number of case and time elements as input.");
          return(NhlFATAL);
        }
      }
      if(datai_n_dims == 3) {
        if(psfc_dimsizes[0] != ntime) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface pressure must have same number of time elements as input.");
          return(NhlFATAL);
        }
      }
      if(psfc_dimsizes[psfc_n_dims-1] != nodes) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface pressure must have same number of nodes elements as input.");
        return(NhlFATAL);
      }
    }
    psf_blk = nodes;
    psf_elem = 1;
    for(i = 0; i < psfc_n_dims; i++) {
      psf_elem *= psfc_dimsizes[i];
    }
    
    intyp = (int*)NclGetArgValue(
                        5,
                        12,
                        NULL,
                        NULL,
                        &intyp_missing,
                        &intyp_has_missing,
                        NULL,
                        0);
    p0_ptr = (void*)NclGetArgValue(
                        6,
                        12,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        &p0_type,
                        0);
    if(p0_type != NCL_double ) {
      p0  = (double*)NclMalloc(sizeof(double));
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,(void*)p0,(void*)p0_ptr,1,
                 NULL,NULL,
                 (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(p0_type))));
    } else {
      p0 = (double*) p0_ptr;
    }
    ilev = (int*)NclGetArgValue(
                        7,
                        12,
                        NULL,
                        NULL,
                        &ilev_missing,
                        &ilev_has_missing,
                        NULL,
                        0);
    kxtrp = (logical*)NclGetArgValue(
                        8,
                        12,
                        NULL,
                        NULL,
                        &kxtrp_missing,
                        &kxtrp_has_missing,
                        NULL,
                        0);


    varflg = (int*)NclGetArgValue(
                        9,
                        12,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

    tbot = (void*)NclGetArgValue(
                        10,
                        12,
                        &tbot_n_dims,
                        tbot_dimsizes,
                        NULL,
                        NULL,
                        &tbot_type,
                        0);


    if(psfc_n_dims != tbot_n_dims ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Temperature at the lowest temperature level must have the same number of dimensions as surface pressure.");
      return(NhlFATAL);
    }
    
    for(i = 0; i < psfc_n_dims; i++) {
      if(psfc_dimsizes[i] != tbot_dimsizes[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Temperature at the lowest temperature level must have the same dimension sizes as surface pressure.");
        return(NhlFATAL);
      }
    }
    
    phis = (void*)NclGetArgValue(
                        11,
                        12,
                        &phis_n_dims,
                        phis_dimsizes,
                        NULL,
                        NULL,
                        &phis_type,
                        0);
    if(phis_n_dims < 1 || phis_n_dims > 3 || phis_n_dims > psfc_n_dims) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface geopotential must either be a 1, 2, or 3-dimensional array, and must have the same or fewer dimensions than surface pressure");
    }
    for(i = 0; i < phis_n_dims; i++) {
      if(phis_dimsizes[i] != psfc_dimsizes[psfc_n_dims-phis_n_dims+i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: Surface geopotential must have the same dimension sizes as the rightmost dimensions of surface pressure.");
        return(NhlFATAL);
      }
    }
    phis_elem = 1;
    for(i = 0; i < phis_n_dims; i++) {
      phis_elem *= phis_dimsizes[i];
    }

    nlevip1 = nlevi+1;

/*
 * Check dimension sizes.
 */
    if((nodes > INT_MAX) || (nlevi > INT_MAX) ||
       (nlevip1 > INT_MAX) || (nlevo > INT_MAX))  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_ecmwf_nodes: one or more dimension sizes is greater than INT_MAX");
      return(NhlFATAL);
    }
    inodes = (int) nodes;
    inlevi = (int) nlevi;
    inlevip1 = (int) nlevip1;
    inlevo = (int) nlevo;

    plevi = (double*)NclMalloc(nlevip1*sizeof(double));
    if(not_double) {
      datao = (char*)NclMalloc(total * nblk_out * sizeof(float));
      tmp_datai = (double*)NclMalloc(nblk * sizeof(double));
      tmp_datao = (double*)NclMalloc(nblk_out* sizeof(double));
      psfc_d = (double*) NclMalloc(psf_blk*sizeof(double));
      tbot_d = (double*) NclMalloc(psf_blk*sizeof(double));
      phis_d = (double*) NclMalloc(psf_blk*sizeof(double));
      if(phis_n_dims == 1) {
        _Nclcoerce((NclTypeClass)nclTypedoubleClass, phis_d,
                   ((char*)phis),psf_blk,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(phis_type))));
      }
/*
 * Loop across leftmost dimensions and pass subsections of the input
 * arrays to the Fortran routine.  Since phis can be 1 to 3
 * dimensions, and psfc, tbot, datai can be 3 to 5 dimensions, we
 * have to loop separately across number of cases and times so
 * we can correctly deal with the subsections of phis, psfc, tbot, datai.
 */
      i = index_phis = 0;
      for(nc = 0; nc < ncase ; nc++) {
        for(nt = 0; nt < ntime ; nt++) {
/*
 * The input is not double, so we have to coerce it.
 */
          _Nclcoerce((NclTypeClass)nclTypedoubleClass, tmp_datai,
                     (datai+i*sz*nblk),nblk,NULL,NULL,datai_md->multidval.type);
          _Nclcoerce((NclTypeClass)nclTypedoubleClass, psfc_d,
                     ((char*)psfc+sz*psf_blk*i),psf_blk,NULL,NULL,
                     (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
          _Nclcoerce((NclTypeClass)nclTypedoubleClass, tbot_d,
                     ((char*)tbot+sz*psf_blk*i),psf_blk,NULL,NULL,
                     (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(tbot_type))));
          if(phis_n_dims >= 2) {
            _Nclcoerce((NclTypeClass)nclTypedoubleClass, phis_d,
                       ((char*)phis+sz*psf_blk*index_phis),psf_blk,NULL,NULL,
                       (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(phis_type))));
          }
	  NGCALLF(dvinth2pecmwfnodes,DVINTH2PECMWFNODES)(tmp_datai,tmp_datao,
					 hbcofa,hbcofb,p0,plevi,(double *)plevo,
					 intyp,ilev,psfc_d,
					 (double *)(&missing),kxtrp,
					 &inodes,&inlevi,&inlevip1,
					 &inlevo,varflg,
					 tbot_d,phis_d);
          for(j = 0; j< nblk_out; j++) {
            ((float*)datao)[i*nblk_out + j] = (float)tmp_datao[j];
          }
          i++;
          index_phis++;
        }
        if(phis_n_dims == 2) {
          index_phis = 0;
        }
      }
      NclFree(psfc_d);
      NclFree(tbot_d);
      NclFree(phis_d);
      NclFree(tmp_datai);
      NclFree(tmp_datao);
      
    } else {
/*
 * The input is already double, which makes the code a little "simpler"
 * here.
 *
 * Create space for datao return array.
 */
      datao = (char*)NclMalloc(total * nblk_out * sizeof(double));
      if(psfc_type != NCL_double) {
        psfc_d = (double*) NclMalloc(psf_elem*sizeof(double));
        _Nclcoerce((NclTypeClass)nclTypedoubleClass, psfc_d, 
                   (char*)psfc,psf_elem,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(psfc_type))));
      } else {
        psfc_d =(double*) psfc;
      }
      if(tbot_type != NCL_double) {
        tbot_d = (double*) NclMalloc(psf_elem*sizeof(double));
        _Nclcoerce((NclTypeClass)nclTypedoubleClass, tbot_d, (char*)tbot,
                   psf_elem,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(tbot_type))));
      } else {
        tbot_d =(double*) tbot;
      }
      if(phis_type != NCL_double) {
        phis_d = (double*) NclMalloc(phis_elem*sizeof(double));
        _Nclcoerce((NclTypeClass)nclTypedoubleClass, phis_d,(char*)phis,
                   phis_elem,NULL,NULL,
                   (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(phis_type))));
      } else {
        phis_d =(double*) phis;
      }
/*
 * Loop across leftmost dimensions and pass subsections of the input
 * arrays to the Fortran routine.  Since phis can 1 to 3 
 * dimensions, and psfc, tbot, datai can be 2 to 4 dimensions, we
 * have to loop separately across number of cases and times so
 * we can correctly deal with the subsections of phis, psfc, tbot, datai.
 */
      i = index_phis = 0;
      for(nc = 0; nc < ncase ; nc++) {
        for(nt = 0; nt < ntime ; nt++) {
          if(phis_n_dims >= 2) {
/*
 * phis_n_dims is 2D and needs to be coerced.
 */
	    NGCALLF(dvinth2pecmwfnodes,DVINTH2PECMWFNODES)(
					   (double *)(datai+sizeof(double)*i*nblk),
					   (double *)(((char*)datao)+sizeof(double)*nblk_out*i),
					   hbcofa,hbcofb,p0,plevi,(double *)plevo,
					   intyp,ilev,
					   (double *)(((char*)psfc_d)+sizeof(double)*psf_blk*i),
					   (double *)(&missing),kxtrp,
					   &inodes,&inlevi,&inlevip1,
					   &inlevo,varflg,
					   (double *)(((char*)tbot_d)+sizeof(double)*psf_blk*i),
					   (double *)(((char*)phis_d)+sizeof(double)*psf_blk*index_phis));
          }
          else {
/*
 * phis_n_dims is 1D and has already been coerced.
 */
	    NGCALLF(dvinth2pecmwfnodes,DVINTH2PECMWFNODES)(
                                          (double *)(datai+sizeof(double)*i*nblk),
					  (double *)(((char*)datao)+sizeof(double)*nblk_out*i),
					  hbcofa,hbcofb,p0,plevi,(double *)plevo,
					  intyp,ilev,
					  (double *)(((char*)psfc_d)+sizeof(double)*psf_blk*i),
					  (double *)(&missing),kxtrp,
					  &inodes,&inlevi,&inlevip1,
					  &inlevo,varflg,
					  (double *)(((char*)tbot_d)+sizeof(double)*psf_blk*i),
					  phis_d);
          }
          i++;
          index_phis++;
        }
        if(phis_n_dims == 2) {
          index_phis = 0;
        }
      }
      if((void*)psfc_d != psfc) {
        NclFree(psfc_d);
      }
      if((void*)tbot_d != tbot) {
        NclFree(tbot_d);
      }
      if((void*)phis_d != phis) {
        NclFree(phis_d);
      }
    }
    

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
 * input was 2D, 3D, or 4D.
 *
 * Start with the 4D case.
 */
      if(datai_n_dims == 4 ) {
        dim_info[0].dim_num   = 0;
        dim_info[0].dim_quark = NrmStringToQuark("case"); 
        dim_info[0].dim_size  = ncase;
        
        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = NrmStringToQuark("time"); 
        dim_info[1].dim_size  = ntime;
        
        dim_info[2].dim_num   = 2;
        dim_info[2].dim_quark = plevo_quark; 
        dim_info[2].dim_size  = nlevo;
        
        dim_info[3].dim_num   = 3;
        dim_info[3].dim_quark = NrmStringToQuark("nodes");
        dim_info[3].dim_size  = nodes;
                  
        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&nlevo,TEMPORARY,NULL,(NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[2]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = -1;
        ids[1] = -1;
        ids[2] = lev_coord_var->obj.id;
        ids[3] = -1;

        datao_dimsizes[0] = ncase;
        datao_dimsizes[1] = ntime;
        datao_dimsizes[2] = nlevo;
        datao_dimsizes[3] = nodes;
        
        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,4,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
      }
      else if(datai_n_dims == 3 ) {
/*
 * Here's the 3D case.
 */
        dim_info[0].dim_num   = 0;
        dim_info[0].dim_quark = NrmStringToQuark("time"); 
        dim_info[0].dim_size  = ntime;

        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = plevo_quark; 
        dim_info[1].dim_size  = nlevo;

        dim_info[2].dim_num   = 2;
        dim_info[2].dim_quark = NrmStringToQuark("nodes");
        dim_info[2].dim_size  = nodes;

        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&nlevo,TEMPORARY,NULL,(NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,&(dim_info[1]),-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = -1;
        ids[1] = lev_coord_var->obj.id;
        ids[2] = -1;

        datao_dimsizes[0] = ntime;
        datao_dimsizes[1] = nlevo;
        datao_dimsizes[2] = nodes;

        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,3,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
      } else {
/*
 * Here's the 2D case.
 */
        dim_info[0].dim_num   = 0;
        dim_info[0].dim_quark = plevo_quark; 
        dim_info[0].dim_size  = nlevo;

        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = NrmStringToQuark("nodes");
        dim_info[1].dim_size  = nodes;

        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,
                                     NULL,1,&nlevo,TEMPORARY,NULL,
                                     (NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,
                                                   NULL,lev_coord_md,dim_info,
                                                   -1,NULL,COORD,
                                                   NrmQuarkToString(plevo_quark),
                                                   TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = lev_coord_var->obj.id;
        ids[1] = -1;
        datao_dimsizes[0] = nlevo;
        datao_dimsizes[1] = nodes;
        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,2,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
        
      }
      data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,
                                      dim_info,-1,ids,RETURNVAR,NULL,
                                      TEMPORARY);
      data.kind = NclStk_VAR;
      _NclPlaceReturn(data);
    } else {
/*
 * The input data may or may not have coordinate information attached
 * to it.
 *
 * There are three different cases here, depending on whether the
 * input was 2D, 3D, or 4D.
 *
 * Start with the 4D case.
 */
      if(datai_n_dims == 4) {
        dim_info[0].dim_num   = 0; 
        dim_info[0].dim_quark = val.u.data_var->var.dim_info[0].dim_quark;
        dim_info[0].dim_size  = ncase;
        
        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = val.u.data_var->var.dim_info[1].dim_quark;
        dim_info[1].dim_size  = ntime;
                  
        dim_info[2].dim_num   = 2;
        dim_info[2].dim_quark = plevo_quark;
        dim_info[2].dim_size  = nlevo;
                  
        dim_info[3].dim_num   = 3;
        dim_info[3].dim_quark = val.u.data_var->var.dim_info[3].dim_quark;
        dim_info[3].dim_size  = nodes;
                  
        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,
                                     NULL,1,&nlevo,TEMPORARY,NULL,
                                     (NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,
                                                   NULL,lev_coord_md,
                                                   &(dim_info[2]),-1,NULL,
                                                   COORD,
                                                   NrmQuarkToString(plevo_quark),
                                                   TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = -1;
        ids[1] = -1;
        ids[2] = lev_coord_var->obj.id;
        ids[3] = -1;
        datao_dimsizes[0] = ncase;
        datao_dimsizes[1] = ntime;
        datao_dimsizes[2] = nlevo;
        datao_dimsizes[3] = nodes;
        
        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,4,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
        data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,
                                        dim_info,-1,ids,RETURNVAR,NULL,
                                        TEMPORARY);

/*
 * Here's where we check if the input variable had any dimension
 * information attached to it. If so, use it to return it with the
 * return output.
 *
 * Since this is the 4D case, we need to check dimensions 0, 1, and 3.
 * (The 2-th dimension is the level dimension that we've already dealt
 * with above.)
 */
        if((val.u.data_var->var.dim_info[0].dim_quark != -1) &&
           (_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,
                                     NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,
                            _NclVarValueRead(tmp_var,NULL,NULL),
                            NrmQuarkToString(data.u.data_var->var.dim_info[0].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[0] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[0]),tmp_var);
          }
        }
                  
        if((val.u.data_var->var.dim_info[1].dim_quark != -1) &&
           (_NclIsCoord(val.u.data_var,
           NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,
                                     NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),
                            NrmQuarkToString(data.u.data_var->var.dim_info[1].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[1] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[1]),tmp_var);
          }
        }
                  
        if((val.u.data_var->var.dim_info[3].dim_quark != -1) &&
           (_NclIsCoord(val.u.data_var,
           NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,
                                     NrmQuarkToString(val.u.data_var->var.dim_info[3].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,
                            _NclVarValueRead(tmp_var,NULL,NULL),
                            NrmQuarkToString(data.u.data_var->var.dim_info[3].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[3] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[3]),tmp_var);
          }
        }
      }
      else if(datai_n_dims == 3) {
        dim_info[0].dim_num   = 0;
        dim_info[0].dim_quark = val.u.data_var->var.dim_info[0].dim_quark;
        dim_info[0].dim_size  = ntime;

        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = plevo_quark;
        dim_info[1].dim_size  = nlevo;

        dim_info[2].dim_num   = 2;
        dim_info[2].dim_quark = val.u.data_var->var.dim_info[2].dim_quark;
        dim_info[2].dim_size  = nodes;

        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,
                                     NULL,1,&nlevo,TEMPORARY,NULL,
                                     (NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,
                                                   NULL,lev_coord_md,
                                                   &(dim_info[1]),-1,NULL,
                                                   COORD,
                                                   NrmQuarkToString(plevo_quark),
                                                   TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = -1;
        ids[1] = lev_coord_var->obj.id;
        ids[2] = -1;

        datao_dimsizes[0] = ntime;
        datao_dimsizes[1] = nlevo;
        datao_dimsizes[2] = nodes;

        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,3,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
        data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,
                                        dim_info,-1,ids,RETURNVAR,NULL,
                                        TEMPORARY);
        
        if((val.u.data_var->var.dim_info[0].dim_quark != -1) &&
           (_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[0].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),
                            NrmQuarkToString(data.u.data_var->var.dim_info[0].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[0] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[0]),tmp_var);
          }
        }
        
        if((val.u.data_var->var.dim_info[2].dim_quark != -1) &&
           (_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[2].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[2] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[2]),tmp_var);
          }
        }
      } else {
        dim_info[0].dim_num   = 0;
        dim_info[0].dim_quark = plevo_quark;
        dim_info[0].dim_size  = nlevo; 

        dim_info[1].dim_num   = 1;
        dim_info[1].dim_quark = val.u.data_var->var.dim_info[1].dim_quark;
        dim_info[1].dim_size  = nodes;

        lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&nlevo,TEMPORARY,NULL,(NclObjClass)plevo_type_class);
        lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,dim_info,-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
        if(!plevo_was_val) {
          _NclAttCopyWrite(lev_coord_var,plevo_val.u.data_var);
        }
        ids[0] = lev_coord_var->obj.id;
        ids[1] = -1;

        datao_dimsizes[0] = nlevo;
        datao_dimsizes[1] = nodes;

        tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,
                               &out_missing,2,datao_dimsizes,TEMPORARY,NULL,
                               not_double ? (NclObjClass)nclTypefloatClass : (NclObjClass)nclTypedoubleClass);
        data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,
                                        dim_info,-1,ids,RETURNVAR,NULL,
                                        TEMPORARY);
        
        if((val.u.data_var->var.dim_info[1].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark)))) {
          tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
          _NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
          if(data.u.data_var->var.coord_vars[1] != -1) {
            _NclAttCopyWrite((NclVar)_NclGetObj(data.u.data_var->var.coord_vars[1]),tmp_var);
          }
        }
      }
      data.kind = NclStk_VAR;
      _NclPlaceReturn(data);
    }
    return(NhlNOERROR);
}

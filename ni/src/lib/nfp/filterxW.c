#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dfiltrq,DFILTRQ)(int*,double*,double*,double*,int*,
                                     double*,double*,double*,int*);

extern void NGCALLF(filwgtnormal,FILWGTNORMAL)(int*,double*,int*,double*,int*);

NhlErrorTypes filwgts_lanczos_W( void )
{
/*
 * Input array variables
 */
  int *nwgt, *ihp;
  void *fca, *fcb, *nsigma;
  double *tmp_fca, *tmp_fcb, *tmp_nsigma;
  NclBasicDataTypes type_fca, type_fcb, type_nsigma;
/*
 * Output array variables
 */
  void *wgt, *freq, *resp;
  double *tmp_wgt, *tmp_freq, *tmp_resp;
  NclBasicDataTypes type_wgt;
  NclTypeClass type_wgt_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  int nfreq, ier, nwgt2;
/*
 * Retrieve arguments.
 */
  nwgt = (int*)NclGetArgValue(
          0,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  if(*nwgt < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: nwgt must be >= 3");
    return(NhlFATAL);
  }
  if(!(*nwgt % 2)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: nwgt must be odd");
    return(NhlFATAL);
  }
  nfreq = (*nwgt*2)+3;
  nwgt2 = *nwgt + 2;    /* We will strip off the first and last
                            points later. */

  ihp = (int*)NclGetArgValue(
          1,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  if(*ihp < 0 || *ihp > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: ihp must be 0 <= ihp <= 2");
    return(NhlFATAL);
  }

  fca = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_fca,
          DONT_CARE);

  fcb = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_fcb,
          DONT_CARE);

  nsigma = (void*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_nsigma,
          DONT_CARE);

/*
 * Coerce fca, fcb, and nsigma to double if necessary.
 */
  tmp_fca    = coerce_input_double(fca,type_fca,1,0,NULL,NULL);
  tmp_fcb    = coerce_input_double(fcb,type_fcb,1,0,NULL,NULL);
  tmp_nsigma = coerce_input_double(nsigma,type_nsigma,1,0,NULL,NULL);
  if(tmp_fca == NULL || tmp_fcb == NULL || tmp_nsigma == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: Unable to allocate memory for coercing input to double");
    return(NhlFATAL);
  }

  if(*tmp_fca < 0. || *tmp_fca > 0.5) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: fca must be 0 <= fca <= 0.5");
    return(NhlFATAL);
  }

  if(*ihp == 2) {
    if(*tmp_fcb < 0. || *tmp_fcb > 0.5) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: fcb must be 0 <= fcb <= 0.5");
      return(NhlFATAL);
    }
    if(*tmp_fcb < *tmp_fca) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: fca must be <= fcb");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output.  We allocate space for tmp_wgt no matter
 * what, because we later need to strip off the first and last point
 * from it.
 */

  tmp_wgt  = (double*)calloc(nwgt2,sizeof(double));
  if(tmp_wgt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  if(type_fca == NCL_double) {
    type_wgt = NCL_double;
    wgt  = (void*)calloc(*nwgt,sizeof(double));
    freq = (void*)calloc(nfreq,sizeof(double));
    resp = (void*)calloc(nfreq,sizeof(double));

    if(wgt == NULL || freq == NULL || resp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
/*
 * Point temp arrays to actual arrays, since they are double.
 */
    tmp_resp = &((double*)resp)[0];
    tmp_freq = &((double*)freq)[0];
  }
  else {
    type_wgt = NCL_float;
    wgt      = (void*)calloc(*nwgt,sizeof(float));
    freq     = (void*)calloc(nfreq,sizeof(float));
    resp     = (void*)calloc(nfreq,sizeof(float));
/*
 * Allocate temp arrays as double, since the originals are only float.
 */
    tmp_freq = (double*)calloc(nfreq,sizeof(double));
    tmp_resp = (double*)calloc(nfreq,sizeof(double));

    if(wgt  == NULL || freq == NULL || resp == NULL || 
       tmp_freq == NULL || tmp_resp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lanczos: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  type_wgt_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_wgt)));
/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(dfiltrq,DFILTRQ)(&nwgt2,tmp_fca,tmp_fcb,tmp_nsigma,ihp,tmp_wgt,
                           tmp_resp,tmp_freq,&ier);
  
/*  
 * We need to strip off the first and last points of tmp_wgt, which
 * is of length *nwgt+2.
 */
  coerce_output_float_or_double(wgt,&tmp_wgt[1],type_wgt,*nwgt,0);

  if(type_wgt == NCL_float) {
    coerce_output_float_only(resp,tmp_resp,nfreq,0);
    coerce_output_float_only(freq,tmp_freq,nfreq,0);
  }

/*
 * Free memory.
 */
  NclFree(tmp_wgt);
  if(type_fca    != NCL_double) NclFree(tmp_fca);
  if(type_fcb    != NCL_double) NclFree(tmp_fcb);
  if(type_nsigma != NCL_double) NclFree(tmp_nsigma);
  if(type_wgt != NCL_double) {
    NclFree(tmp_freq);
    NclFree(tmp_resp);
  }

/*
 * Get ready to return all this stuff to NCL.
 *
 * freq and resp are returned as attributes.
 */
  dsizes[0] = *nwgt;
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            wgt,
                            NULL,
                            1,
                            dsizes,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_wgt_class
                            );

/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  dsizes[0] = nfreq;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         freq,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_wgt_class
                         );
  _NclAddAtt(
             att_id,
             "freq",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         resp,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_wgt_class
                         );
  _NclAddAtt(
             att_id,
             "resp",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
    return_data.kind = NclStk_VAR;
    return_data.u.data_var = tmp_var;
    _NclPlaceReturn(return_data);
    return(NhlNOERROR);
}

NhlErrorTypes filwgts_normal_W( void )
{
/*
 * Input array variables
 */
  int *nwgt, *option;
  void *sigma;
  double *tmp_sigma;
  NclBasicDataTypes type_sigma;
/*
 * Output array variables
 */
  void *wgt;
  double *tmp_wgt;
  NclBasicDataTypes type_wgt;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t dsizes[1];
  int ier;
/*
 * Retrieve arguments.
 */
  nwgt = (int*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  sigma = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_sigma,
          DONT_CARE);

  option = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Coerce sigma to double if necessary.
 */
  tmp_sigma = coerce_input_double(sigma,type_sigma,1,0,NULL,NULL);
  if(tmp_sigma == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_normal: Unable to allocate memory for coercing input to double");
    return(NhlFATAL);
  }

  if(type_sigma != NCL_double) {
    tmp_wgt  = (double*)calloc(*nwgt,sizeof(double));
    wgt      = (void*)calloc(*nwgt,sizeof(float));
    type_wgt = NCL_float;
    if(tmp_wgt == NULL || wgt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_normal: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    wgt      = (void*)calloc(*nwgt,sizeof(double));
    type_wgt = NCL_double;
    if(wgt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_normal: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_wgt = (double*)wgt;
  }

/*
 * option is isn't used for anything yet, so default it to zero. 
 */
  *option = 0;
/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(filwgtnormal,FILWGTNORMAL)(nwgt,tmp_wgt,option,tmp_sigma,&ier);
  
  if(type_wgt == NCL_float) {
    coerce_output_float_only(wgt,tmp_wgt,*nwgt,0);
  }

/*
 * Free memory.
 */
  if(type_wgt != NCL_double) {
    NclFree(tmp_wgt);
  }
  if(type_sigma != NCL_double) {
    NclFree(tmp_sigma);
  }
/*
 * Return.
 */
  dsizes[0] = *nwgt;
  return(NclReturnValue(wgt,1,dsizes,NULL,type_wgt,0));
}

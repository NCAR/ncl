#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <math.h>

extern void NGCALLF(dfiltrq,DFILTRQ)(int*,double*,double*,int*,int*,
                                     double*,double*,double*,int*);

NhlErrorTypes filwgts_lancos_W( void )
{
/*
 * Input array variables
 */
  int *nwgt, *ihp, *nsigma;
  void *fca, *fcb;
  double *tmp_fca, *tmp_fcb;
  NclBasicDataTypes type_fca, type_fcb;
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
  int att_id, dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  int i, j, nfreq, ier, nwgt2;
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
          2);

  if(*nwgt < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: nwgt must be >= 3");
    return(NhlFATAL);
  }
  if(!(*nwgt % 2)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: nwgt must be odd");
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
          2);

  if(*ihp < 0 || *ihp > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: ihp must be 0 <= ihp <= 2");
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
          2);

  fcb = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_fcb,
          2);

  nsigma = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Coerce fca and fcb to double if necessary.
 */
  tmp_fca = coerce_input_double(fca,type_fca,1,0,NULL,NULL);
  tmp_fcb = coerce_input_double(fcb,type_fcb,1,0,NULL,NULL);
  if(tmp_fca == NULL || tmp_fcb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for coercing input to double");
    return(NhlFATAL);
  }

  if(*tmp_fca < 0. || *tmp_fca > 0.5) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: fca must be 0 <= fca <= 0.5");
    return(NhlFATAL);
  }

  if(*ihp == 2) {
    if(*tmp_fcb < 0. || *tmp_fcb > 0.5) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: fcb must be 0 <= fcb <= 0.5");
      return(NhlFATAL);
    }
    if(*tmp_fcb < *tmp_fca) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: fca must be <= fcb");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  if(type_fca == NCL_double) {
    type_wgt = NCL_double;
    wgt  = (void*)calloc(*nwgt,sizeof(double));
    freq = (void*)calloc(nfreq,sizeof(double));
    resp = (void*)calloc(nfreq,sizeof(double));

    if(wgt == NULL || freq == NULL || resp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for output arrays");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  type_wgt_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_wgt)));
/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(dfiltrq,DFILTRQ)(&nwgt2,tmp_fca,tmp_fcb,nsigma,ihp,tmp_wgt,
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

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
  int dsizes_wgt[1];
  NclBasicDataTypes type_wgt;
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
  int i, j, nf, ier;
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: nwgt must be > 3");
    return(NhlFATAL);
  }
  nf = (*nwgt*2)-1;

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
 * Allocate space for output.
 */
  if(type_fca == NCL_double) {
    type_wgt = NCL_double;
    wgt  = (void*)calloc(*nwgt,sizeof(double));
    freq = (void*)calloc(nf,sizeof(double));
    resp = (void*)calloc(nf,sizeof(double));

    if(wgt == NULL || freq == NULL || resp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
/*
 * Point temp arrays to actual arrays, since they are double.
 */
    tmp_resp = &((double*)resp)[0];
    tmp_freq = &((double*)freq)[0];
    tmp_wgt  = &((double*)wgt)[0];
  }
  else {
    type_wgt = NCL_float;
    wgt      = (void*)calloc(*nwgt,sizeof(float));
    freq     = (void*)calloc(nf,sizeof(float));
    resp     = (void*)calloc(nf,sizeof(float));
/*
 * Allocate temp arrays as double, since the originals are only float.
 */
    tmp_wgt  = (double*)calloc(*nwgt,sizeof(double));
    tmp_freq = (double*)calloc(nf,sizeof(double));
    tmp_resp = (double*)calloc(nf,sizeof(double));

    if(wgt  == NULL || tmp_wgt  == NULL ||
       freq == NULL || tmp_freq == NULL ||
       resp == NULL || tmp_resp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"filwgts_lancos: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  dsizes_wgt[0] = *nwgt;
/*
 * Call the Fortran version of this routine.
 */
  NGCALLF(dfiltrq,DFILTRQ)(nwgt,tmp_fca,tmp_fcb,nsigma,ihp,tmp_wgt,tmp_resp,
                           tmp_freq,&ier);
  
  if(type_wgt == NCL_float) {
    for(j = 0; j < *nwgt; j++) {
      ((float*)wgt)[j] = (float)(tmp_wgt[j]);
    }
    for(j = 0; j < nf; j++) {
      ((float*)resp)[j] = (float)(tmp_resp[j]);
      ((float*)freq)[j] = (float)(tmp_freq[j]);
    }
  }
/*
 * Free memory.
 */
  if(type_wgt != NCL_double) {
    NclFree(tmp_wgt);
    NclFree(tmp_freq);
    NclFree(tmp_resp);
  }

/*
 * Get ready to return all this stuff to NCL.
 *
 * freq and resp are returned as attributes.
 */
  if(type_wgt == NCL_float) {
/*
 * Input is not double, so return float values.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              wgt,
                              NULL,
                              1,
                              dsizes_wgt,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
  }
  else {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              wgt,
                              NULL,
                              1,
                              dsizes_wgt,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
  }
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  dsizes[0] = nf;
/*
 * Set up float attribute to return.
 */
  if(type_wgt == NCL_float) {
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
                           (NclObjClass)nclTypefloatClass
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
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "resp",
               att_md,
               NULL
               );
  }
  else {
/*
 * Set up double attribute to return.
 */
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
                           (NclObjClass)nclTypedoubleClass
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
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "resp",
               att_md,
               NULL
               );
  }
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

#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
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

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dezffti,DEZFFTI)(int*,double*);
extern void NGCALLF(dezfftf,DEZFFTF)(int*,double*,double*,double*,double*,
                                     double*);
extern void NGCALLF(dezfftb,DEZFFTB)(int*,double*,double*,double*,double*,
                                     double*);

NhlErrorTypes ezfftf_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  float *rcf;
  double *dcf;
  int dsizes_cf[2];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Attribute variables
 */
  float *rxbar;
  double *dxbar;
/*
 * various
 */
  double *work;
  int i, npts, npts2, npts22;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           2);
/*
 * Calculate size of output array.
 */
  npts   = dsizes_x[0];
  npts2  = npts/2;
  npts22 = 2*npts2;
  dsizes_cf[0] = 2;
  dsizes_cf[1] = npts2;
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,npts,0,NULL,NULL);
  if ( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
  dxbar = (double *)calloc(1,sizeof(double));
  dcf   = (double*)calloc(npts22,sizeof(double));
  if ( dcf == NULL || dxbar == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for output values" );
    return(NhlFATAL);
  }

/*
 * Allocate memory for work array
 */
  work = (double*)calloc((3*npts+15),sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'dezfftf' with the full argument list.
 */
  NGCALLF(dezffti,DEZFFTI)(&npts,work);
  NGCALLF(dezfftf,DEZFFTF)(&npts,dx,dxbar,&dcf[0],&dcf[npts2],work);
/*
 * Free up memory.
 */
  NclFree(work);
  if((void*)dx != x) NclFree(dx);
/*
 * Set up variable to return.
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    rcf   = (float*)calloc(npts22,sizeof(float));
    rxbar = (float*)calloc(1,sizeof(float));
    if( rcf == NULL || rxbar == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Unable to allocate memory for output values");
      return(NhlFATAL);
    }
/*
 * Copy double values to float.
 */
    for( i = 0; i < npts22; i++ ) rcf[i] = (float)dcf[i];
    *rxbar = (float)*dxbar;
/*
 * Free up double precision versions of these variables.
 */
    NclFree(dcf);
    NclFree(dxbar);
/*
 * Set up return values.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)rcf,
                        NULL,
                        2,
                        dsizes_cf,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Attributes
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)rxbar,
                        NULL,
                        1,
                        dsizes,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
    _NclAddAtt(
               att_id,
               "xbar",
               att_md,
               NULL
               );
  }
  else {
/*
 * Input was double, so return double output.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)dcf,
                        NULL,
                        2,
                        dsizes_cf,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypedoubleClass
                        );
/*
 * Attributes
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
        
    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)dxbar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "xbar",
               att_md,
               NULL
               );

  }
/*
 * Set up variable to hold return array and attributes.
 */
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


NhlErrorTypes ezfftb_W( void )
{
/*
 * Input array variables
 */
  void *cf;
  double *dcf;
  float *rcf;
  int ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS];
  void *xbar;
  double *dxbar;
  NclBasicDataTypes type_cf, type_xbar;
/*
 * Output array variables
 */
  float *rx;
  double *dx;
  int dsizes_x[1];
/*
 * various
 */
  double *work;
  int i, npts, npts2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  cf = (void*)NclGetArgValue(
           0,
           2,
           &ndims_cf, 
           dsizes_cf,
           NULL,
           NULL,
           &type_cf,
           2);
  xbar = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xbar,
           2);
/*
 * Calculate size of output array.
 */
  npts2 = dsizes_cf[1];
  npts  = 2*npts2;
  dsizes_x[0] = npts;
/*
 * Coerce input data to double if necessary.
 */
  dcf = coerce_input_double(cf,type_cf,npts,0,NULL,NULL);
  dxbar = coerce_input_double(xbar,type_xbar,1,0,NULL,NULL);
  if (dcf == NULL || dxbar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for output array.
 */
  dx = (double*)calloc(npts,sizeof(double));
  if ( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for output array" );
    return(NhlFATAL);
  }

/*
 * Allocate memory for work array
 */
  work = (double*)calloc((3*npts+15),sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'dezfftb' with the full argument list.
 */
  NGCALLF(dezffti,DEZFFTI)(&npts,work);
  NGCALLF(dezfftb,DEZFFTB)(&npts,dx,dxbar,&dcf[0],&dcf[npts2],work);
/*
 * Free up memory.
 */
  NclFree(work);

  if((void*)dcf != cf) NclFree(dcf);

  if(type_cf != NCL_double) {
/*
 * Copy double values to float values.
 */
    rx = (float*)calloc(npts,sizeof(float));
    if( rx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < npts; i++ ) rx[i] = (float)dx[i];

/* 
 * Free up double precision array.
 */
    NclFree(dx);
/*
 * return float values 
 */
    return(NclReturnValue((void*)rx,1,dsizes_x,NULL,NCL_float,0));
  }
  else {
/*
 * return double values
 */
    return(NclReturnValue((void*)dx,1,dsizes_x,NULL,NCL_double,0));
  }
}


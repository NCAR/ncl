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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  double *tmp_x;
/*
 * Output array variables
 */
  void *cf, *xbar;
  int ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS];
  double *tmp_cf1, *tmp_cf2, *tmp_xbar;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  double *work;
  int i, j, npts, npts2, lnpts2, npts22, index_x, index_cf;
  int size_leftmost, size_cf;
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
 * Calculate number of leftmost elements.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];
/*
 * Calculate size of output array.
 */
  npts   = dsizes_x[ndims_x-1];
  npts2  = npts/2;
  lnpts2 = npts2 * size_leftmost;
  npts22 = 2*npts2;
  size_cf = size_leftmost * npts22;

  ndims_cf           = ndims_x + 1;
  dsizes_cf[0]       = 2;
  for(i = 1; i < ndims_x; i++ ) dsizes_cf[i] = dsizes_x[i-1];
  dsizes_cf[ndims_x] = npts2;
/*
 * Create space for temporary input array if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output arrays.
 */
  tmp_xbar = (double*)calloc(1,sizeof(double));
  tmp_cf1  = (double*)calloc(npts2,sizeof(double));
  tmp_cf2  = (double*)calloc(npts2,sizeof(double));
  if ( tmp_cf1 == NULL || tmp_cf2 == NULL || tmp_xbar == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for temporary output arrays" );
    return(NhlFATAL);
  }
  if(type_x == NCL_double) {
    cf   = (void*)calloc(size_cf,sizeof(double));
    xbar = (void*)calloc(size_leftmost,sizeof(double));
  }
  else {
    cf   = (void*)calloc(size_cf,sizeof(float));
    xbar = (void*)calloc(size_leftmost,sizeof(float));
  }
  if ( cf == NULL || xbar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for output arrays" );
    return(NhlFATAL);
  }

/*
 * Allocate memory for work array
 */
  work = (double*)calloc((4*npts+15),sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'dezfftf' with the full argument list.
 */
  index_x = index_cf = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) { 
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    NGCALLF(dezffti,DEZFFTI)(&npts,work);
    NGCALLF(dezfftf,DEZFFTF)(&npts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into cf.
 */
    if(type_x == NCL_double) {
      ((double*)xbar)[i] = *tmp_xbar;
      for(j = 0; j < npts2; j++) {
        ((double*)cf)[index_cf+j]        = tmp_cf1[j];
        ((double*)cf)[lnpts2+index_cf+j] = tmp_cf2[j];
      }
    }
    else {
      ((float*)xbar)[i] = (float)*tmp_xbar;
      for(j = 0; j < npts2; j++) {
        ((float*)cf)[index_cf+j]        = (float)tmp_cf1[j];
        ((float*)cf)[lnpts2+index_cf+j] = (float)tmp_cf2[j];
      }
    }
    index_x  += npts;
    index_cf += npts2;
  }
/*
 * Free up memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  NclFree(work);
  NclFree(tmp_cf1);
  NclFree(tmp_cf2);
  NclFree(tmp_xbar);
/*
 * Set up variable to return.
 */
  if(type_x != NCL_double) {
/*
 * Set up return values.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        cf,
                        NULL,
                        ndims_cf,
                        dsizes_cf,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Attributes
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = size_leftmost;
    att_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        xbar,
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
                        cf,
                        NULL,
                        ndims_cf,
                        dsizes_cf,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypedoubleClass
                        );
/*
 * Attributes
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
        
    dsizes[0] = size_leftmost;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           xbar,
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
  double *tmp_cf1, *tmp_cf2;
  int ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS], dsizes_xbar[1];
  void *xbar;
  double *tmp_xbar;
  NclBasicDataTypes type_cf, type_xbar;
/*
 * Output array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
/*
 * various
 */
  double *work;
  int i, j, npts, npts2, lnpts2, index_cf, index_x, size_x, size_leftmost;
  int scalar_xbar;
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
           dsizes_xbar,
           NULL,
           NULL,
           &type_xbar,
           2);
/*
 * Calculate number of leftmost elements.
 */
  size_leftmost = 1;
  for( i = 1; i < ndims_cf-1; i++ ) size_leftmost *= dsizes_cf[i];
/*
 * Check xbar dimension sizes.
 */
  scalar_xbar = is_scalar(1,dsizes_xbar);

  if(!scalar_xbar) {
/*
 * If xbar is not a scalar, it must be an array of the same dimension
 * sizes as the leftmost dimensions of cf (except the first dimension
 * of '2').
 */ 
    if(dsizes_xbar[0] != size_leftmost) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: If xbar is not a scalar, then it must be a single vector of the length of the product of the leftmost dimensions of 'cf' (not including the '2' dimension)") ;
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of output array.
 */
  npts2  = dsizes_cf[ndims_cf-1];
  npts   = 2*npts2;
  lnpts2 = npts2 * size_leftmost;
  size_x = size_leftmost * npts;

  ndims_x = ndims_cf - 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_x[i] = dsizes_cf[i+1];
  dsizes_x[ndims_x-1] = npts;
/*
 * Create arrays to coerce input to double if necessary.
 */
  if(type_cf != NCL_double) {
    tmp_cf1 = (double*)calloc(npts2,sizeof(double));
    tmp_cf2 = (double*)calloc(npts2,sizeof(double));
    if(tmp_cf1 == NULL || tmp_cf2 == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_xbar != NCL_double) {
    tmp_xbar = (double*)calloc(1,sizeof(double));
    if(tmp_xbar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for output array.
 */
  tmp_x = (double *)calloc(npts,sizeof(double));
  if (tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for temporary output array" );
    return(NhlFATAL);
  }
  if(type_cf == NCL_double) {
    type_x = NCL_double;
    x = (void*)calloc(size_x,sizeof(double));
  }
  else {
    type_x = NCL_float;
    x = (void*)calloc(size_x,sizeof(float));
  }
  if (x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for output array" );
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array
 */
  work = (double*)calloc(4*npts+15,sizeof(double));
  if ( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for work array" );
    return(NhlFATAL);
  }
/*
 * If xbar is a scalar, coerce it outside the loop.
 */
  if(scalar_xbar) {
    if(type_xbar != NCL_double) { 
      coerce_subset_input_double(xbar,tmp_xbar,0,type_xbar,1,0,NULL,NULL);
    }
    else {
      tmp_xbar = &((double*)xbar)[0];
    }
  }

/*
 * Call the f77 version of 'dezfftb' with the full argument list.
 */
  index_x = index_cf = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_cf != NCL_double) { 
      coerce_subset_input_double(cf,tmp_cf1,index_cf,type_cf,npts2,0,
                                 NULL,NULL);
      coerce_subset_input_double(cf,tmp_cf2,lnpts2+index_cf,type_cf,npts2,0,
                                 NULL,NULL);
    }
    else {
      tmp_cf1 = &((double*)cf)[index_cf];
      tmp_cf2 = &((double*)cf)[lnpts2+index_cf];
    }
/*
 * If xbar is not a scalar, then we need to coerce each element
 * to double or else just grab its value.
 */
    if(!scalar_xbar) {
      if(type_xbar != NCL_double) { 
        coerce_subset_input_double(xbar,tmp_xbar,i,type_xbar,1,0,NULL,NULL);
      }
      else {
        tmp_xbar = &((double*)xbar)[i];
      }
    }

    NGCALLF(dezffti,DEZFFTI)(&npts,work);
    NGCALLF(dezfftb,DEZFFTB)(&npts,tmp_x,tmp_xbar,tmp_cf1,tmp_cf2,work);
/*
 * Copy results back into x.
 */
    if(type_cf == NCL_double) {
      for(j = 0; j < npts; j++) {
        ((double*)x)[index_x+j] = tmp_x[j];
      }
    }
    else {
      for(j = 0; j < npts; j++) {
        ((float*)x)[index_x+j] = (float)(tmp_x[j]);
      }
    }
    index_x  += npts;
    index_cf += npts2;
  }
/*
 * Free up memory.
 */
  if(type_cf != NCL_double) {
    NclFree(tmp_cf1);
    NclFree(tmp_cf2);
  }
  if(type_xbar != NCL_double) NclFree(tmp_xbar);
  NclFree(tmp_x);
  NclFree(work);

  return(NclReturnValue(x,ndims_x,dsizes_x,NULL,type_x,0));
}


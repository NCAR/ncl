#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"
#include <math.h>

extern void NGCALLF(dwgtareaave,DWGTAREAAVE)(double*,double*,double*,int*,
                                             int*,double*,int*,double*);

extern void NGCALLF(dwgtvolave,DWGTVOLAVE)(double*,double*,double*,double*,
                                           int*,int*,int*,double*,int*,
                                           double*);

extern void NGCALLF(wgtvolavehy,WGTVOLAVEHY)(double*,double*,double*,double*,
                                             int*,int*,int*,double*,int*,
                                             double*);

NhlErrorTypes wgt_areaave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgty, *wgtx;
  double *tmp_x, *tmp_wgty, *tmp_wgtx, *tmp1_wgtx, *tmp1_wgty;
  int *iflag;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_wgtx, dsizes_wgtx[NCL_MAX_DIMENSIONS];
  int ndims_wgty, dsizes_wgty[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave;
  int *dsizes_ave, ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int i, index_x, nx, ny, nxny, total_leftmost;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          4,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  wgty = (void*)NclGetArgValue(
          1,
          4,
          &ndims_wgty,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          2);

  wgtx = (void*)NclGetArgValue(
          2,
          4,
          &ndims_wgtx,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          2);

  iflag = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions.
 */
  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: the input array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nxny = nx * ny;

  if(dsizes_wgty[ndims_wgty-1] != 1 && dsizes_wgty[ndims_wgty-1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgtx[ndims_wgtx-1] != 1 && dsizes_wgtx[ndims_wgtx-1] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the size of the output array.
 */
  ndims_ave  = max(1,ndims_x-2);
  dsizes_ave = (int*)calloc(ndims_ave,sizeof(int));  
  if( dsizes_ave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_ave[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_x-2; i++ ) {
    dsizes_ave[i] = dsizes_x[i];
    total_leftmost *= dsizes_x[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxny,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,nx,0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,ny,0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,ndims_wgtx,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,ndims_wgty,dsizes_wgty,ny);

  if(tmp_wgtx == NULL || tmp_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output.
 */
  if(type_x != NCL_double) {
    ave     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_ave = (double*)calloc(1,sizeof(double));
    if(tmp_ave == NULL || ave == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    ave = (void*)calloc(total_leftmost,sizeof(double));
    if( ave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nxny,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    if(type_x == NCL_double) tmp_ave = &((double*)ave)[i];

    NGCALLF(dwgtareaave,DWGTAREAAVE)(tmp_x,tmp_wgty,tmp_wgtx,&nx,&ny,
                                     &missing_dx.doubleval,iflag,tmp_ave);
    if(type_x != NCL_double) {
      ((float*)ave)[i] = (float)*tmp_ave;
    }
    else {
      ((double*)ave)[i] = *tmp_ave;
    }

    index_x += nxny;
  }
/*
 * Free stuff.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_ave);
  }
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);

  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
        
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes wgt_volave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgtz, *wgty, *wgtx;
  double *tmp_x; 
  double *tmp_wgtz, *tmp_wgty, *tmp_wgtx, *tmp1_wgtz, *tmp1_wgtx, *tmp1_wgty;
  int *iflag;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_wgtx, dsizes_wgtx[NCL_MAX_DIMENSIONS];
  int ndims_wgty, dsizes_wgty[NCL_MAX_DIMENSIONS];
  int ndims_wgtz, dsizes_wgtz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty, type_wgtz;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave;
  int *dsizes_ave, ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int i, index_x, nx, ny, nz, nxnynz, total_leftmost;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  wgtz = (void*)NclGetArgValue(
          1,
          5,
          &ndims_wgtz,
          dsizes_wgtz,
          NULL,
          NULL,
          &type_wgtz,
          2);

  wgty = (void*)NclGetArgValue(
          2,
          5,
          &ndims_wgty,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          2);

  wgtx = (void*)NclGetArgValue(
          3,
          5,
          &ndims_wgtx,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          2);

  iflag = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions.
 */
  if(ndims_x < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: the input array must have at least 3 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nz = dsizes_x[ndims_x-3];
  nxnynz = nx * ny * nz;

  if(dsizes_wgtz[ndims_wgtz-1] != 1 && dsizes_wgtz[ndims_wgtz-1] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgtz must be a scalar or a 1-dimensional vector the same size as the third-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgty[ndims_wgty-1] != 1 && dsizes_wgty[ndims_wgty-1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgtx[ndims_wgtx-1] != 1 && dsizes_wgtx[ndims_wgtx-1] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the size of the output array.
 */
  ndims_ave  = max(1,ndims_x-3);
  dsizes_ave = (int*)calloc(ndims_ave,sizeof(int));  
  if( dsizes_ave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_ave[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_x-3; i++ ) {
    dsizes_ave[i] = dsizes_x[i];
    total_leftmost *= dsizes_x[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxnynz,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,nx,0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,ny,0,NULL,NULL);
  tmp1_wgtz = coerce_input_double(wgtz,type_wgtz,nz,0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL || tmp1_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,ndims_wgtx,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,ndims_wgty,dsizes_wgty,ny);
  tmp_wgtz = copy_scalar_to_array(tmp1_wgtz,ndims_wgtz,dsizes_wgtz,nz);

  if(tmp_wgtx == NULL || tmp_wgty == NULL || tmp_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output.
 */
  if(type_x != NCL_double) {
    ave     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_ave = (double*)calloc(1,sizeof(double));
    if(tmp_ave == NULL || ave == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    ave = (void*)calloc(total_leftmost,sizeof(double));
    if( ave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nxnynz,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    if(type_x == NCL_double) tmp_ave = &((double*)ave)[i];

    NGCALLF(dwgtvolave,DWGTVOLAVE)(tmp_x,tmp_wgtz,tmp_wgty,tmp_wgtx,&nx,&ny,
                                   &nz,&missing_dx.doubleval,iflag,tmp_ave);
    if(type_x != NCL_double) {
      ((float*)ave)[i] = (float)*tmp_ave;
    }
    else {
      ((double*)ave)[i] = *tmp_ave;
    }

    index_x += nxnynz;
  }
/*
 * Free stuff.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_ave);
  }
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);
  if(tmp_wgtz != tmp1_wgtz) NclFree(tmp_wgtz);

  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
  if(type_wgtz != NCL_double) NclFree(tmp1_wgtz);

  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0));
  }
}


NhlErrorTypes wgt_volave_ccm_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgtz, *wgty, *wgtx;
  double *tmp_x; 
  double *tmp_wgtz, *tmp_wgty, *tmp_wgtx, *tmp1_wgtx, *tmp1_wgty;
  int *iflag;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_wgtx, dsizes_wgtx[NCL_MAX_DIMENSIONS];
  int ndims_wgty, dsizes_wgty[NCL_MAX_DIMENSIONS];
  int ndims_wgtz, dsizes_wgtz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty, type_wgtz;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave;
  int *dsizes_ave, ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int i, index_x, nx, ny, nz, nxnynz, total_leftmost;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          5,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  wgtz = (void*)NclGetArgValue(
          1,
          5,
          &ndims_wgtz,
          dsizes_wgtz,
          NULL,
          NULL,
          &type_wgtz,
          2);

  wgty = (void*)NclGetArgValue(
          2,
          5,
          &ndims_wgty,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          2);

  wgtx = (void*)NclGetArgValue(
          3,
          5,
          &ndims_wgtx,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          2);

  iflag = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Check dimensions.
 */
  if(ndims_x < 3 || ndims_wgtz != ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: x must have at least 3 dimensions, and the dimensions of x and wgtz must be the same");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_wgtz[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: wgtz must be the same dimensions as x");
      return(NhlFATAL);
    }
  }

/*
 * Compute the size of the output array.
 */
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nz = dsizes_x[ndims_x-3];
  nxnynz = nx * ny * nz;

  ndims_ave  = max(1,ndims_x-3);
  dsizes_ave = (int*)calloc(ndims_ave,sizeof(int));  
  if( dsizes_ave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_ave[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_x-3; i++ ) {
    dsizes_ave[i] = dsizes_x[i];
    total_leftmost *= dsizes_x[i];
  }

/*
 * wgty must either be a scalar or a 1-d array of length ny.
 * wgtx must either be a scalar or a 1-d array of length nx.
 */
  if(dsizes_wgty[ndims_wgty-1] != 1 && dsizes_wgty[ndims_wgty-1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgtx[ndims_wgtx-1] != 1 && dsizes_wgtx[ndims_wgtx-1] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of x");
    return(NhlFATAL);
  }

/*
 * Coerce the missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Create a temporary array to hold subarrays of x and wgtz.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxnynz,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_wgtz != NCL_double) {
    tmp_wgtz = (double*)calloc(nxnynz,sizeof(double));
    if( tmp_wgtz == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for coercing wgtz array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,nx,0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,ny,0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,ndims_wgtx,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,ndims_wgty,dsizes_wgty,ny);

  if(tmp_wgtx == NULL || tmp_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output.
 */
  if(type_x != NCL_double) {
    ave     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_ave = (double*)calloc(1,sizeof(double));
    if(tmp_ave == NULL || ave == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    ave = (void*)calloc(total_leftmost,sizeof(double));
    if( ave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_x = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nxnynz,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }

    if(type_wgtz != NCL_double) {
/*
 * Coerce subsection of wgtz (tmp_wgtz) to double.
 */
      coerce_subset_input_double(wgtz,tmp_wgtz,index_x,type_wgtz,nxnynz,0,
                                 NULL,NULL);
    }
    else {
      tmp_wgtz = &((double*)wgtz)[index_x];
    }

    if(type_x == NCL_double) tmp_ave = &((double*)ave)[i];

    NGCALLF(wgtvolavehy,WGTVOLAVEHY)(tmp_x,tmp_wgtz,tmp_wgty,tmp_wgtx,&nx,&ny,
                                     &nz,&missing_dx.doubleval,iflag,tmp_ave);
    if(type_x != NCL_double) {
      ((float*)ave)[i] = (float)*tmp_ave;
    }
    else {
      ((double*)ave)[i] = *tmp_ave;
    }

    index_x += nxnynz;
  }
/*
 * Free stuff.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_ave);
  }
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);

  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
  if(type_wgtz != NCL_double) NclFree(tmp_wgtz);

  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0));
  }
}



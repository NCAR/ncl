#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dwgtareaave,DWGTAREAAVE)(double*,double*,double*,int*,
                                             int*,double*,int*,double*);

extern void NGCALLF(dwgtareasum2,DWGTAREASUM2)(double*,double*,int*,int*,
                                               double*,int*,double*);

extern void NGCALLF(dwgtareaave2,DWGTAREAAVE2)(double*,double*,int*,int*,
                                               double*,int*,double*);

extern void NGCALLF(dwgtarearmse2,DWGTAREARMSE2)(double*,double*,double*,
                                                 int*,int*,double*,double*,
                                                 int*,double*);

extern void NGCALLF(dwgtvolave,DWGTVOLAVE)(double*,double*,double*,double*,
                                           int*,int*,int*,double*,int*,
                                           double*);

extern void NGCALLF(dwgtvolaveccm,DWGTVOLAVECCM)(double*,double*,double*,
                                                 double*,int*,int*,int*,
                                                 double*,int*,double*);

extern void NGCALLF(dwgtarearmse,DWGTAREARMSE)(double*,double*,double*,
                                               double*,int*,int*,double*,
                                               double*,int*,double*);

extern void NGCALLF(dwgtvolrmse,DWGTVOLRMSE)(double*,double*,double*,
                                             double*,double*,int*,
                                             int*,int*,double*,double*,int*,
                                             double*);

extern void NGCALLF(dwgtvolrmseccm,DWGTVOLRMSECCM)(double*,double*,double*,
                                                   double*,double*,double*,
                                                   int*,int*,int*,double*,
                                                   double*,int*,double*);

NhlErrorTypes wgt_areaave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgty, *wgtx;
  double *tmp_x = NULL;
  double *tmp_wgty = NULL;
  double *tmp_wgtx = NULL;
  double *tmp1_wgtx = NULL;
  double *tmp1_wgty = NULL;
  int *iflag;
  int ndims_x;
  int has_missing_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave = NULL;
  ng_size_t *dsizes_ave;
  int ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  ng_size_t nxny, total_leftmost;
  int nx, ny;
  ng_size_t index_x;
  int ret;

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
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: the input array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nxny = nx * ny;

  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }

  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of x");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_ave  = max(1,ndims_x-2);
  dsizes_ave = (ng_size_t *)calloc(ndims_ave,sizeof(ng_size_t));  
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
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,ny);

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
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ave);
  return(ret);
}


NhlErrorTypes wgt_areaave2_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgt;
  double *tmp_x = NULL;
  double *tmp_wgt = NULL;
  int *iflag;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  ng_size_t dsizes_wgt[2];
  NclBasicDataTypes type_x, type_wgt;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave = NULL;
  ng_size_t *dsizes_ave;
  int ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t index_x;
  int nx, ny;
  ng_size_t i;
  ng_size_t nxny, total_leftmost;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: the input array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nxny = nx * ny;

  if(dsizes_wgt[0] != ny && dsizes_wgt[1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: wgt must be a 2-dimensional array the same size as the two rightmost dimensions of x");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_ave  = max(1,ndims_x-2);
  dsizes_ave = (ng_size_t *)calloc(ndims_ave,sizeof(ng_size_t));  
  if( dsizes_ave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: Unable to allocate memory for holding dimension sizes");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp_wgt = coerce_input_double(wgt,type_wgt,nxny,0,NULL,NULL);

  if(tmp_wgt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output.
 */
  if(type_x != NCL_double) {
    ave     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_ave = (double*)calloc(1,sizeof(double));
    if(tmp_ave == NULL || ave == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    ave = (void*)calloc(total_leftmost,sizeof(double));
    if( ave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areaave2: Unable to allocate memory for output array");
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

    NGCALLF(dwgtareaave2,DWGTAREAAVE2)(tmp_x,tmp_wgt,&nx,&ny,
                                       &missing_dx.doubleval,iflag,tmp_ave);
    if(type_x != NCL_double) ((float*)ave)[i] = (float)*tmp_ave;

    index_x += nxny;
  }
/*
 * Free stuff.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_ave);
  }
  if(type_wgt != NCL_double) NclFree(tmp_wgt);

  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ave);
  return(ret);
}


NhlErrorTypes wgt_areasum2_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgt;
  double *tmp_x = NULL;
  double *tmp_wgt = NULL;
  int *iflag;
  int ndims_x;
  int has_missing_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgt[2];
  NclBasicDataTypes type_x, type_wgt;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *sum;
  double *tmp_sum = NULL;
  ng_size_t *dsizes_sum;
  int ndims_sum;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t index_x;
  int nx, ny;
  ng_size_t i;
  ng_size_t nxny, total_leftmost;

/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          3,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: the input array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_x[ndims_x-1];
  ny = dsizes_x[ndims_x-2];
  nxny = nx * ny;

  if(dsizes_wgt[0] != ny && dsizes_wgt[1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: wgt must be a 2-dimensional array the same size as the two rightmost dimensions of x");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_sum  = max(1,ndims_x-2);
  dsizes_sum = (ng_size_t *)calloc(ndims_sum,sizeof(ng_size_t));  
  if( dsizes_sum == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_sum[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_x-2; i++ ) {
    dsizes_sum[i] = dsizes_x[i];
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp_wgt = coerce_input_double(wgt,type_wgt,nxny,0,NULL,NULL);

  if(tmp_wgt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output.
 */
  if(type_x != NCL_double) {
    sum     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_sum = (double*)calloc(1,sizeof(double));
    if(tmp_sum == NULL || sum == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    sum = (void*)calloc(total_leftmost,sizeof(double));
    if( sum == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_areasum2: Unable to allocate memory for output array");
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

    if(type_x == NCL_double) tmp_sum = &((double*)sum)[i];

    NGCALLF(dwgtareasum2,DWGTAREASUM2)(tmp_x,tmp_wgt,&nx,&ny,
                                       &missing_dx.doubleval,iflag,tmp_sum);
    if(type_x != NCL_double) ((float*)sum)[i] = (float)*tmp_sum;

    index_x += nxny;
  }
/*
 * Free stuff.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_sum);
  }
  if(type_wgt != NCL_double) NclFree(tmp_wgt);

  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(sum,ndims_sum,dsizes_sum,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(sum,ndims_sum,dsizes_sum,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_sum);
  return(ret);
}


NhlErrorTypes wgt_arearmse_W( void )
{
/*
 * Input array variables
 */
  void *q, *r, *wgty, *wgtx;
  double *tmp_q = NULL;
  double *tmp_r = NULL;
  double *tmp_wgty = NULL;
  double *tmp_wgtx = NULL;
  double *tmp1_wgtx = NULL;
  double *tmp1_wgty = NULL;
  int *iflag;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int has_missing_q;
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_r;
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_q, type_r, type_wgtx, type_wgty;
  NclScalar missing_q, missing_dq, missing_rq;
  NclScalar missing_r, missing_dr;
/*
 * Output variable.
 */
  void *rmse;
  double *tmp_rmse = NULL;
  ng_size_t *dsizes_rmse;
  int ndims_rmse;
  NclBasicDataTypes type_rmse;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  ng_size_t index_q;
  int nlon, nlat;
  ng_size_t nlatnlon;
  ng_size_t total_leftmost;
  int ret;

/*
 * Retrieve arguments.
 */
  q = (void*)NclGetArgValue(
          0,
          5,
          &ndims_q,
          dsizes_q,
          &missing_q,
          &has_missing_q,
          &type_q,
          DONT_CARE);

  r = (void*)NclGetArgValue(
          1,
          5,
          &ndims_r,
          dsizes_r,
          &missing_r,
          &has_missing_r,
          &type_r,
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_q < 2 || ndims_q != ndims_r) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: the first two input arrays must have at least 2 dimensions and be the same size");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_q; i++ ) {
    if(dsizes_q[i] != dsizes_r[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: the first two input arrays must be the same size");
        return(NhlFATAL);
    }
  }
  nlat = dsizes_q[ndims_q-2];
  nlon = dsizes_q[ndims_q-1];
  nlatnlon = nlon * nlat;

  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of q");
    return(NhlFATAL);
  }

  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of q");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_rmse  = max(1,ndims_q-2);
  dsizes_rmse = (ng_size_t *)calloc(ndims_rmse,sizeof(ng_size_t));  
  if( dsizes_rmse == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_rmse[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_q-2; i++ ) {
    dsizes_rmse[i] = dsizes_q[i];
    total_leftmost *= dsizes_q[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,NULL);
/*
 * Create temporary arrays to hold subarrays of q and r.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_r != NCL_double) {
    tmp_r = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nlon);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,nlat);

  if(tmp_wgtx == NULL || tmp_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output.
 */
  if(type_q != NCL_double && type_r != NCL_double) {
    type_rmse = NCL_float;
    rmse     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_rmse = (double*)calloc(1,sizeof(double));
    if(tmp_rmse == NULL || rmse == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_rmse = NCL_double;
    rmse = (void*)calloc(total_leftmost,sizeof(double));
    if( rmse == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_q = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce subsection of q (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,nlatnlon,
                                 has_missing_q,&missing_q,&missing_dq);
    }
    else {
      tmp_q = &((double*)q)[index_q];
    }

    if(type_r != NCL_double) {
/*
 * Coerce subsection of r (tmp_r) to double.
 */
      coerce_subset_input_double(r,tmp_r,index_q,type_r,nlatnlon,
                                 has_missing_r,&missing_r,&missing_dr);
    }
    else {
      tmp_r = &((double*)r)[index_q];
    }

    if(type_rmse == NCL_double) tmp_rmse = &((double*)rmse)[i];

    NGCALLF(dwgtarearmse,DWGTAREARMSE)(tmp_r,tmp_q,tmp_wgty,tmp_wgtx,&nlon,
                                       &nlat,&missing_dr.doubleval,
                                       &missing_dq.doubleval,iflag,tmp_rmse);
    if(type_rmse != NCL_double) {
      ((float*)rmse)[i] = (float)*tmp_rmse;
    }

    index_q += nlatnlon;
  }
/*
 * Free stuff.
 */
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_r != NCL_double) NclFree(tmp_r);
  if(type_rmse != NCL_double) NclFree(tmp_rmse);
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);

  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
        
  if(type_rmse != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_rq,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_dq,NCL_double,0);
  }
  NclFree(dsizes_rmse);
  return(ret);
}


NhlErrorTypes wgt_arearmse2_W( void )
{
/*
 * Input array variables
 */
  void *q, *r, *wgt;
  double *tmp_q = NULL;
  double *tmp_r = NULL;
  double *tmp_wgt = NULL;
  int *iflag;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int has_missing_q;
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_r;
  ng_size_t dsizes_wgt[2];
  NclBasicDataTypes type_q, type_r, type_wgt;
  NclScalar missing_q, missing_dq, missing_rq;
  NclScalar missing_r, missing_dr;
/*
 * Output variable.
 */
  void *rmse;
  double *tmp_rmse = NULL;
  ng_size_t *dsizes_rmse;
  int ndims_rmse;
  NclBasicDataTypes type_rmse;
/*
 * Declare various variables for random purposes.
 */
  int ret, nx, ny;
  ng_size_t i, index_q;
  ng_size_t nxny, total_leftmost;

/*
 * Retrieve arguments.
 */
  q = (void*)NclGetArgValue(
          0,
          4,
          &ndims_q,
          dsizes_q,
          &missing_q,
          &has_missing_q,
          &type_q,
          DONT_CARE);

  r = (void*)NclGetArgValue(
          1,
          4,
          &ndims_r,
          dsizes_r,
          &missing_r,
          &has_missing_r,
          &type_r,
          DONT_CARE);

  wgt = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_wgt,
          NULL,
          NULL,
          &type_wgt,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_q < 2 || ndims_r < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: the input arrays must have at least 2 dimensions");
    return(NhlFATAL);
  }
  nx = dsizes_q[ndims_q-1];
  ny = dsizes_q[ndims_q-2];
  nxny = nx * ny;

  if(ndims_q != ndims_r) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: The first two input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_q; i++ ) {
    if(dsizes_q[i] != dsizes_r[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: The first two input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

  if(dsizes_wgt[0] != ny && dsizes_wgt[1] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: wgt must be a 2-dimensional array the same size as the two rightmost dimensions of x and y");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_rmse  = max(1,ndims_q-2);
  dsizes_rmse = (ng_size_t *)calloc(ndims_rmse,sizeof(ng_size_t));  
  if( dsizes_rmse == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_rmse[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_q-2; i++ ) {
    dsizes_rmse[i] = dsizes_q[i];
    total_leftmost *= dsizes_q[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dr,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dq,NULL);
/*
 * Create a temporary array to hold subarrays of q and r.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(nxny,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_r != NCL_double) {
    tmp_r = (double*)calloc(nxny,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp_wgt = coerce_input_double(wgt,type_wgt,nxny,0,NULL,NULL);

  if(tmp_wgt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output.
 */
  if(type_q == NCL_double || type_r == NCL_double) {
    type_rmse = NCL_double;
    rmse = (void*)calloc(total_leftmost,sizeof(double));
    if( rmse == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_rmse = NCL_float;
    rmse     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_rmse = (double*)calloc(1,sizeof(double));
    if(tmp_rmse == NULL || rmse == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_arearmse2: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_q = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce subsection of q (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,nxny,has_missing_q,
                                 &missing_q,&missing_dq);
    }
    else {
      tmp_q = &((double*)q)[index_q];
    }

    if(type_r != NCL_double) {
/*
 * Coerce subsection of r (tmp_r) to double.
 */
      coerce_subset_input_double(r,tmp_r,index_q,type_r,nxny,has_missing_r,
                                 &missing_r,&missing_dr);
    }
    else {
      tmp_r = &((double*)r)[index_q];
    }

    if(type_rmse == NCL_double) tmp_rmse = &((double*)rmse)[i];

    NGCALLF(dwgtarearmse2,DWGTAREARMSE2)(tmp_q,tmp_r,tmp_wgt,&nx,&ny,
                                         &missing_dq.doubleval,
                                         &missing_dr.doubleval,iflag,tmp_rmse);
    if(type_rmse != NCL_double) ((float*)rmse)[i] = (float)*tmp_rmse;

    index_q += nxny;
  }
/*
 * Free stuff.
 */
  if(type_q   != NCL_double) NclFree(tmp_q);
  if(type_r   != NCL_double) NclFree(tmp_r);
  if(type_wgt != NCL_double) NclFree(tmp_wgt);
  if(type_rmse!= NCL_double) NclFree(tmp_rmse);


  if(type_q != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_rq,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_dq,NCL_double,0);
  }
  NclFree(dsizes_rmse);
  return(ret);
}


NhlErrorTypes wgt_volave_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgtz, *wgty, *wgtx;
  double *tmp_x = NULL; 
  double *tmp_wgtz = NULL;
  double *tmp_wgty = NULL;
  double *tmp_wgtx = NULL;
  double *tmp1_wgtz = NULL;
  double *tmp1_wgtx = NULL;
  double *tmp1_wgty = NULL;
  int *iflag;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgtz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty, type_wgtz;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave = NULL;
  ng_size_t *dsizes_ave;
  int ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t i, index_x;
  int nx, ny, nz;
  ng_size_t nxnynz, total_leftmost;

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
          DONT_CARE);

  wgtz = (void*)NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_wgtz,
          NULL,
          NULL,
          &type_wgtz,
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
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

  if(dsizes_wgtz[0] != 1 && dsizes_wgtz[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgtz must be a scalar or a 1-dimensional vector the same size as the third-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of x");
    return(NhlFATAL);
  }
/*
 * Compute the size of the output array.
 */
  ndims_ave  = max(1,ndims_x-3);
  dsizes_ave = (ng_size_t *)calloc(ndims_ave,sizeof(ng_size_t));  
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
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);
  tmp1_wgtz = coerce_input_double(wgtz,type_wgtz,dsizes_wgtz[0],0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL || tmp1_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,ny);
  tmp_wgtz = copy_scalar_to_array(tmp1_wgtz,1,dsizes_wgtz,nz);

  if(tmp_wgtx == NULL || tmp_wgty == NULL || tmp_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave: Unable to allocate memory for coercing weights to double precision");
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
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ave);
  return(ret);
}


NhlErrorTypes wgt_volave_ccm_W( void )
{
/*
 * Input array variables
 */
  void *x, *wgtz, *wgty, *wgtx;
  double *tmp_x = NULL; 
  double *tmp_wgtz = NULL;
  double *tmp_wgty = NULL;
  double *tmp_wgtx = NULL;
  double *tmp1_wgtx = NULL;
  double *tmp1_wgty = NULL;
  int *iflag;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  int ndims_wgtz;
  ng_size_t dsizes_wgtz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x, type_wgtx, type_wgty, type_wgtz;
  NclScalar missing_x, missing_dx, missing_rx;
/*
 * Output variable.
 */
  void *ave;
  double *tmp_ave = NULL;
  ng_size_t *dsizes_ave;
  int ndims_ave;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  int nx, ny, nz;
  ng_size_t i, index_x;
  ng_size_t nxnynz, total_leftmost;

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
          DONT_CARE);

  wgtz = (void*)NclGetArgValue(
          1,
          5,
          &ndims_wgtz,
          dsizes_wgtz,
          NULL,
          NULL,
          &type_wgtz,
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
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
  dsizes_ave = (ng_size_t *)calloc(ndims_ave,sizeof(ng_size_t ));  
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
  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of x");
    return(NhlFATAL);
  }
  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nx) {
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
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nx);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,ny);

  if(tmp_wgtx == NULL || tmp_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volave_ccm: Unable to allocate memory for coercing weights to double precision");
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

    NGCALLF(dwgtvolaveccm,DWGTVOLAVECCM)(tmp_x,tmp_wgtz,tmp_wgty,tmp_wgtx,
                                         &nx,&ny,&nz,&missing_dx.doubleval,
                                         iflag,tmp_ave);
    if(type_x != NCL_double) {
      ((float*)ave)[i] = (float)*tmp_ave;
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
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ave,ndims_ave,dsizes_ave,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ave);
  return(ret);
}


NhlErrorTypes wgt_volrmse_W( void )
{
/*
 * Input array variables
 */
  void *q, *r, *wgtz, *wgty, *wgtx;
  double *tmp_q = NULL;
  double *tmp_r = NULL;
  double *tmp_wgtz, *tmp_wgty, *tmp_wgtx, *tmp1_wgtx, *tmp1_wgty, *tmp1_wgtz;
  int *iflag;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int has_missing_q;
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_r;
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgtz[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_q, type_r, type_wgtx, type_wgty, type_wgtz;
  NclScalar missing_q, missing_dq, missing_rq, missing_r, missing_dr;
/*
 * Output variable.
 */
  void *rmse;
  double *tmp_rmse = NULL;
  ng_size_t *dsizes_rmse;
  int ndims_rmse;
  NclBasicDataTypes type_rmse;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  ng_size_t klevnlatnlon, total_leftmost, index_q;
  int ret;
  int  klev, nlon, nlat;

/*
 * Retrieve arguments.
 */
  q = (void*)NclGetArgValue(
          0,
          6,
          &ndims_q,
          dsizes_q,
          &missing_q,
          &has_missing_q,
          &type_q,
          DONT_CARE);

  r = (void*)NclGetArgValue(
          1,
          6,
          &ndims_r,
          dsizes_r,
          &missing_r,
          &has_missing_r,
          &type_r,
          DONT_CARE);

  wgtz = (void*)NclGetArgValue(
          2,
          6,
          NULL,
          dsizes_wgtz,
          NULL,
          NULL,
          &type_wgtz,
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          4,
          6,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_q < 3 || ndims_q != ndims_r) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: the first two input arrays must have at least 3 dimensions and be the same size");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_q; i++ ) {
    if(dsizes_q[i] != dsizes_r[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: the first two input arrays must be the same size");
        return(NhlFATAL);
    }
  }
  nlon = dsizes_q[ndims_q-1];
  nlat = dsizes_q[ndims_q-2];
  klev = dsizes_q[ndims_q-3];
  klevnlatnlon = klev * nlon * nlat;

  if(dsizes_wgtz[0] != 1 && dsizes_wgtz[0] != klev) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: wgtz must be a scalar or a 1-dimensional vector the same size as the third-to-the-last dimension of q");
    return(NhlFATAL);
  }

  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of q");
    return(NhlFATAL);
  }

  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of q");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_rmse  = max(1,ndims_q-3);
  dsizes_rmse = (ng_size_t *)calloc(ndims_rmse,sizeof(ng_size_t));  
  if( dsizes_rmse == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_rmse[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_q-3; i++ ) {
    dsizes_rmse[i] = dsizes_q[i];
    total_leftmost *= dsizes_q[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,NULL);
/*
 * Create a temporary array to hold subarrays of q.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_r != NCL_double) {
    tmp_r = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);
  tmp1_wgtz = coerce_input_double(wgtz,type_wgtz,dsizes_wgtz[0],0,NULL,NULL);

  if(tmp1_wgtx == NULL || tmp1_wgty == NULL || tmp1_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nlon);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,nlat);
  tmp_wgtz = copy_scalar_to_array(tmp1_wgtz,1,dsizes_wgtz,klev);

  if(tmp_wgtx == NULL || tmp_wgty == NULL || tmp_wgtz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output.
 */
  if(type_q != NCL_double && type_r != NCL_double) {
    type_rmse = NCL_float;
    rmse     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_rmse = (double*)calloc(1,sizeof(double));
    if(tmp_rmse == NULL || rmse == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_rmse = NCL_double;
    rmse = (void*)calloc(total_leftmost,sizeof(double));
    if( rmse == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_q = 0;
  for( i = 0; i < total_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce subsection of q (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,klevnlatnlon,
                                 has_missing_q,&missing_q,&missing_dq);
    }
    else {
      tmp_q = &((double*)q)[index_q];
    }

    if(type_r != NCL_double) {
/*
 * Coerce subsection of r (tmp_r) to double.
 */
      coerce_subset_input_double(r,tmp_r,index_q,type_r,klevnlatnlon,
                                 has_missing_r,&missing_r,&missing_dr);
    }
    else {
      tmp_r = &((double*)r)[index_q];
    }

    if(type_rmse == NCL_double) tmp_rmse = &((double*)rmse)[i];

    NGCALLF(dwgtvolrmse,DWGTVOLRMSE)(tmp_r,tmp_q,tmp_wgtz,tmp_wgty,tmp_wgtx,
                                     &nlon,&nlat,&klev,&missing_dr.doubleval,
                                     &missing_dq.doubleval,iflag,tmp_rmse);
    if(type_rmse != NCL_double) {
      ((float*)rmse)[i] = (float)*tmp_rmse;
    }

    index_q += klevnlatnlon;
  }
/*
 * Free stuff.
 */
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_r != NCL_double) NclFree(tmp_r);
  if(type_rmse != NCL_double) NclFree(tmp_rmse);
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);
  if(tmp_wgtz != tmp1_wgtz) NclFree(tmp_wgtz);

  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
  if(type_wgtz != NCL_double) NclFree(tmp1_wgtz);
        
  if(type_rmse != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_rq,
                         NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_dq,
                         NCL_double,0);
  }
  NclFree(dsizes_rmse);
  return(ret);
}


NhlErrorTypes wgt_volrmse_ccm_W( void )
{
/*
 * Input array variables
 */
  void *q, *r, *wgtq, *wgtr, *wgty, *wgtx;
  double *tmp_q = NULL;
  double *tmp_r = NULL;
  double *tmp_wgtq = NULL;
  double *tmp_wgtr = NULL;
  double *tmp_wgty, *tmp_wgtx, *tmp1_wgtx, *tmp1_wgty;
  int *iflag;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int has_missing_q;
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_r;
  int ndims_wgtq;
  ng_size_t dsizes_wgtq[NCL_MAX_DIMENSIONS];
  int ndims_wgtr;
  ng_size_t dsizes_wgtr[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgtx[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wgty[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_q, type_r, type_wgtq, type_wgtr;
  NclBasicDataTypes type_wgtx, type_wgty;
  NclScalar missing_q, missing_dq, missing_rq, missing_r, missing_dr;
/*
 * Output variable.
 */
  void *rmse;
  double *tmp_rmse = NULL;
  ng_size_t *dsizes_rmse;
  int ndims_rmse;
  NclBasicDataTypes type_rmse;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i;
  ng_size_t klevnlatnlon, total_leftmost;
  int ret, klev, nlon, nlat;
  ng_size_t index_q;

/*
 * Retrieve arguments.
 */
  q = (void*)NclGetArgValue(
          0,
          7,
          &ndims_q,
          dsizes_q,
          &missing_q,
          &has_missing_q,
          &type_q,
          DONT_CARE);

  r = (void*)NclGetArgValue(
          1,
          7,
          &ndims_r,
          dsizes_r,
          &missing_r,
          &has_missing_r,
          &type_r,
          DONT_CARE);

  wgtq = (void*)NclGetArgValue(
          2,
          7,
          &ndims_wgtq,
          dsizes_wgtq,
          NULL,
          NULL,
          &type_wgtq,
          DONT_CARE);

  wgtr = (void*)NclGetArgValue(
          3,
          7,
          &ndims_wgtr,
          dsizes_wgtr,
          NULL,
          NULL,
          &type_wgtr,
          DONT_CARE);

  wgty = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          dsizes_wgty,
          NULL,
          NULL,
          &type_wgty,
          DONT_CARE);

  wgtx = (void*)NclGetArgValue(
          5,
          7,
          NULL,
          dsizes_wgtx,
          NULL,
          NULL,
          &type_wgtx,
          DONT_CARE);

  iflag = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check dimensions.
 */
  if(ndims_q < 3 || ndims_q != ndims_r || ndims_q != ndims_wgtr ||
     ndims_q != ndims_wgtq) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: the first four input arrays must have at least 3 dimensions and be the same size");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_q; i++ ) {
    if(dsizes_q[i] != dsizes_wgtq[i] || dsizes_q[i] != dsizes_wgtr[i] ||
       dsizes_q[i] != dsizes_r[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: the first four input arrays must be the same size");
        return(NhlFATAL);
    }
  }

  nlon = dsizes_q[ndims_q-1];
  nlat = dsizes_q[ndims_q-2];
  klev = dsizes_q[ndims_q-3];
  klevnlatnlon = klev * nlon * nlat;

  if(dsizes_wgty[0] != 1 && dsizes_wgty[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: wgty must be a scalar or a 1-dimensional vector the same size as the second-to-the-last dimension of q");
    return(NhlFATAL);
  }

  if(dsizes_wgtx[0] != 1 && dsizes_wgtx[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: wgtx must be a scalar or a 1-dimensional vector the same size as the last dimension of q");
    return(NhlFATAL);
  }

/*
 * Compute the size of the output array.
 */
  ndims_rmse  = max(1,ndims_q-3);
  dsizes_rmse = (ng_size_t *)calloc(ndims_rmse,sizeof(ng_size_t));  
  if( dsizes_rmse == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  dsizes_rmse[0] = 1;

  total_leftmost = 1;
  for( i = 0; i < ndims_q-3; i++ ) {
    dsizes_rmse[i] = dsizes_q[i];
    total_leftmost *= dsizes_q[i];
  }
/*
 * Coerce the missing value.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,NULL);
/*
 * Create temporary arrays to hold subarrays of q and r.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_r != NCL_double) {
    tmp_r = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_wgtq != NCL_double) {
    tmp_wgtq = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_wgtq == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_wgtr != NCL_double) {
    tmp_wgtr = (double*)calloc(klevnlatnlon,sizeof(double));
    if( tmp_wgtr == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce weights to double if necessary.
 */
  tmp1_wgtx = coerce_input_double(wgtx,type_wgtx,dsizes_wgtx[0],0,NULL,NULL);
  tmp1_wgty = coerce_input_double(wgty,type_wgty,dsizes_wgty[0],0,NULL,NULL);
  if(tmp1_wgtx == NULL || tmp1_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

  tmp_wgtx = copy_scalar_to_array(tmp1_wgtx,1,dsizes_wgtx,nlon);
  tmp_wgty = copy_scalar_to_array(tmp1_wgty,1,dsizes_wgty,nlat);

  if(tmp_wgtx == NULL || tmp_wgty == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for coercing weights to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output.
 */
  if(type_q != NCL_double && type_r != NCL_double) {
    type_rmse = NCL_float;
    rmse     = (void*)calloc(total_leftmost,sizeof(float));
    tmp_rmse = (double*)calloc(1,sizeof(double));
    if(tmp_rmse == NULL || rmse == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_rmse = NCL_double;
    rmse = (void*)calloc(total_leftmost,sizeof(double));
    if( rmse == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_volrmse_ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_q = 0;

  for( i = 0; i < total_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce subsection of q (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,klevnlatnlon,
                                 has_missing_q,&missing_q,&missing_dq);
    }
    else {
      tmp_q = &((double*)q)[index_q];
    }

    if(type_r != NCL_double) {
/*
 * Coerce subsection of r (tmp_r) to double.
 */
      coerce_subset_input_double(r,tmp_r,index_q,type_r,klevnlatnlon,
                                 has_missing_r,&missing_r,&missing_dr);
    }
    else {
      tmp_r = &((double*)r)[index_q];
    }
    
    if(type_wgtq != NCL_double) {
/*
 * Coerce subsection of wgtq (tmp_wgtq) to double.
 */
      coerce_subset_input_double(wgtq,tmp_wgtq,index_q,type_wgtq,
                                 klevnlatnlon,0,NULL,NULL);
    }
    else {
      tmp_wgtq = &((double*)wgtq)[index_q];
    }
    
    if(type_wgtr != NCL_double) {
/*
 * Coerce subsection of wgtr (tmp_wgtr) to double.
 */
      coerce_subset_input_double(wgtr,tmp_wgtr,index_q,type_wgtr,
                                 klevnlatnlon,0,NULL,NULL);
    }
    else {
      tmp_wgtr = &((double*)wgtr)[index_q];
    }

    if(type_rmse == NCL_double) tmp_rmse = &((double*)rmse)[i];

    NGCALLF(dwgtvolrmseccm,DWGTVOLRMSECCM)(tmp_r,tmp_q,tmp_wgtr,tmp_wgtq,
                                           tmp_wgty,tmp_wgtx,&nlon,&nlat,
                                           &klev,&missing_dr.doubleval,
                                           &missing_dq.doubleval,iflag,
                                           tmp_rmse);
    if(type_rmse != NCL_double) {
      ((float*)rmse)[i] = (float)*tmp_rmse;
    }
    index_q += klevnlatnlon;
  }
/*
 * Free stuff.
 */
  if(type_q   != NCL_double) NclFree(tmp_q);
  if(type_r   != NCL_double) NclFree(tmp_r);
  if(type_wgtq != NCL_double) NclFree(tmp_wgtq);
  if(type_wgtr != NCL_double) NclFree(tmp_wgtr);
  if(type_rmse != NCL_double) NclFree(tmp_rmse);
  if(tmp_wgtx != tmp1_wgtx) NclFree(tmp_wgtx);
  if(tmp_wgty != tmp1_wgty) NclFree(tmp_wgty);
  if(type_wgtx != NCL_double) NclFree(tmp1_wgtx);
  if(type_wgty != NCL_double) NclFree(tmp1_wgty);
        
  if(type_rmse != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_rq,
                         NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(rmse,ndims_rmse,dsizes_rmse,&missing_dq,
                         NCL_double,0);
  }
  NclFree(dsizes_rmse);
  return(ret);
}


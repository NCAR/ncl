#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dcfindif,DCFINDIF)(double *,double *,int *,double *,
                                       double *,int *,int *, double *,
                                       double *,int *,double *,int *);

extern void NGCALLF(dvrfidf,DVRFIDF)(double *,double *,double *,double *,
                                     int *,int *,double *,int *,double *,
                                     int *);

extern void NGCALLF(ddvfidf,DDVFIDF)(double *,double *,double *,double *,
                                     int *,int *,double *,int *,double *,
                                     int *);

NhlErrorTypes center_finite_diff_W( void )
{
/*
 * Input array variables
 */
  void *q, *r;
  logical *cyclic;
  int *opt, r_one_d, r_scalar;
  double *tmp_q = NULL;
  double *tmp_r = NULL;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_q, has_missing_r;
  NclScalar missing_q, missing_dq, missing_rq;
  NclScalar missing_r, missing_dr;
  NclBasicDataTypes type_q, type_r, type_dqdr;
/*
 * Output array variables
 */
  void *dqdr;
  double *tmp_dqdr = NULL;
  NclScalar missing_dqdr;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, npts, npts1, size_q, size_leftmost, index_q;
  int inpts, inpts1, iend, ier;
  double *qq, *rr;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
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

  cyclic = (logical*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Get size of input array and test dimension sizes.
 */
  npts  = dsizes_q[ndims_q-1];
  npts1 = npts + 1;

  if((npts > INT_MAX) || (npts1 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: npts1 = %ld is larger than INT_MAX", npts1);
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inpts1 = (int) npts1;

  if((ndims_r == 1 && (dsizes_r[0] != npts && dsizes_r[0] != 1)) ||
     (ndims_r > 1 && ndims_r != ndims_q)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: r must either be a scalar, a 1D array the same length as the rightmost dimemsion of q, or the same size as q");
    return(NhlFATAL);
  }

  if(ndims_r > 1) {
    r_one_d = 0;
    for( i = 0; i < ndims_r-1; i++ ) {
      if(dsizes_r[i] != dsizes_q[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: r must either be a scalar, a 1D array the same length as the rightmost dimemsion of q, or the same size as q");
        return(NhlFATAL);
      }
    }
  }
  else {
    r_one_d = 1;
  }
/*
 * Compute the total size of the q array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_q-1; i++ ) size_leftmost *= dsizes_q[i];
  size_q = size_leftmost * npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,NULL);
/*
 * Create arrays to hold temporary r and q values.
 */
  qq    = (double*)calloc(npts+2,sizeof(double));
  rr    = (double*)calloc(npts+2,sizeof(double));
  if( qq == NULL || rr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }
/*
 * Create temporary arrays to hold double precision data.
 */
  if(type_q != NCL_double) {
    tmp_q = (double*)calloc(npts,sizeof(double));
    if( tmp_q == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for coercing q to double precision");
      return(NhlFATAL);
    }
  }
/*
 * 'r' can be a scalar, one-dimensional, or multi-dimensional.
 * If it is a scalar, then we need to construct an npts-sized 'r'
 * that is based on the scalar value.
 */
  r_scalar = is_scalar(ndims_r,dsizes_r);
  if(type_r != NCL_double || r_scalar) {
    tmp_r = (double*)calloc(npts,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for coercing r to double precision");
      return(NhlFATAL);
    }
/*
 * Coerce r (tmp_r) to double if necessary.
 */
    if(r_one_d) {
      coerce_subset_input_double(r,tmp_r,0,type_r,dsizes_r[0],0,NULL,NULL);
    }
/*
 * If r is a scalar, then copy it npts-1 times to rest of the array.
 */
    if(r_scalar) {
      for(i = 1; i < npts; i++ ) tmp_r[i] = tmp_r[i-1] + tmp_r[0];
    }
  }
  if(type_r == NCL_double && !r_scalar && r_one_d) {
/*
 * Point tmp_r to r.
 */
    tmp_r = &((double*)r)[0];
  }
/*
 * Allocate space for output array.
 */
  if(type_q == NCL_double || type_r == NCL_double) {
    type_dqdr = NCL_double;
    dqdr      = (void*)calloc(size_q,sizeof(double));
    missing_dqdr = missing_dq;
  }
  else {
    type_dqdr = NCL_float;
    dqdr      = (void*)calloc(size_q,sizeof(float));
    tmp_dqdr  = coerce_output_double(dqdr,type_dqdr,npts);
    if( tmp_dqdr == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
    missing_dqdr = missing_rq;
  }
  if( dqdr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for output array");
    return(NhlFATAL);
  }


  if(*cyclic) {
    iend = 0;
  }
  else {
    iend = 1;
  }

/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_q = 0;
  for(i = 0; i < size_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce q (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to q.
 */
      tmp_q = &((double*)q)[index_q];
    }
    if(!r_one_d) {
      if(type_r != NCL_double) {
/*
 * Coerce r (tmp_r) to double.
 */
        coerce_subset_input_double(r,tmp_r,index_q,type_r,npts,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_r to r.
 */
        tmp_r = &((double*)r)[index_q];
      }
    }
    if(type_dqdr == NCL_double) {
/*
 * Point tmp_dqdr to dqdr.
 */
      tmp_dqdr = &((double*)dqdr)[index_q];
    }

/*
 * Call the Fortran routine.
 */
    NGCALLF(dcfindif,DCFINDIF)(tmp_q,tmp_r,&inpts,&missing_dq.doubleval,
                               &missing_dr.doubleval,cyclic,&iend,
                               qq,rr,&inpts1,tmp_dqdr,&ier);

    if(type_dqdr != NCL_double) {
      coerce_output_float_only(dqdr,tmp_dqdr,npts,index_q);
    }
    index_q += npts;
  }
/*
 * Free temp arrays.
 */
  if(type_r != NCL_double || r_scalar) NclFree(tmp_r);
  if(type_q != NCL_double)             NclFree(tmp_q);
  if(type_dqdr != NCL_double)          NclFree(tmp_dqdr);
  NclFree(qq);
  NclFree(rr);

  if(has_missing_q) {
    return(NclReturnValue(dqdr,ndims_q,dsizes_q,&missing_dqdr,type_dqdr,0));
  }
  else {
    return(NclReturnValue(dqdr,ndims_q,dsizes_q,NULL,type_dqdr,0));
  }
}


NhlErrorTypes center_finite_diff_n_W( void )
{
/*
 * Input array variables
 */
  void *q, *r;
  logical *cyclic;
  int *opt, *dim, r_one_d;
  int r_scalar = 1;
  double *tmp_q, *tmp_r;
  int ndims_q;
  ng_size_t dsizes_q[NCL_MAX_DIMENSIONS];
  int ndims_r;
  ng_size_t dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_q, has_missing_r;
  NclScalar missing_q, missing_dq, missing_rq;
  NclScalar missing_r, missing_dr;
  NclBasicDataTypes type_q, type_r, type_dqdr;
/*
 * Output array variables
 */
  void *dqdr;
  double *tmp_dqdr;
  NclScalar missing_dqdr;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, npts, npts1, size_q, size_leftmost, size_rightmost, size_rl;
  ng_size_t index_nrnpts, index_q;
  int inpts, inpts1, iend, ier;
  double *qq, *rr;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
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

  cyclic = (logical*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  dim = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_q) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: Invalid dimension index for calculating the center finite difference");
    return(NhlFATAL);
  }

/*
 * Set value for cyclic.
 */
  if(*cyclic) {
    iend = 0;
  }
  else {
    iend = 1;
  }

/*
 * Get size of input array and test dimension sizes.
 */
  npts  = dsizes_q[*dim];
  npts1 = npts + 1;

  if((npts > INT_MAX) || (npts1 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: npts1 = %ld is larger than INT_MAX", npts1);
    return(NhlFATAL);
  }
  inpts = (int) npts;
  inpts1 = (int) npts1;

  if((ndims_r == 1 && (dsizes_r[0] != npts && dsizes_r[0] != 1)) ||
     (ndims_r > 1 && ndims_r != ndims_q)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: r must either be a scalar, a 1D array the same length as the dim-th dimemsion of q, or the same size as q");
    return(NhlFATAL);
  }

  if(ndims_r > 1) {
    for( i = 0; i < ndims_r; i++ ) {
      if(dsizes_r[i] != dsizes_q[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: r must either be a scalar, a 1D array the same length as the dim-th dimemsion of q, or the same size as q");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total size of the q array.
 */
  size_rightmost = size_leftmost = 1;
  for( i =      0; i < *dim;    i++ ) size_leftmost  *= dsizes_q[i];
  for( i = *dim+1; i < ndims_q; i++ ) size_rightmost *= dsizes_q[i];
  size_rl = size_leftmost * size_rightmost;
  size_q = size_rl * npts;

/*
 * Check for missing values.
 */
  coerce_missing(type_q,has_missing_q,&missing_q,&missing_dq,&missing_rq);
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,NULL);
/*
 * Create arrays to hold temporary r and q values.
 */
  qq = (double*)calloc(npts+2,sizeof(double));
  rr = (double*)calloc(npts+2,sizeof(double));
  if( qq == NULL || rr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }
/*
 * Create temporary arrays to hold double precision data.
 */
  tmp_q = (double*)calloc(npts,sizeof(double));
  if( tmp_q == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: Unable to allocate memory for coercing q to double precision");
    return(NhlFATAL);
  }
/*
 * 'r' can be a scalar, one-dimensional, or multi-dimensional.
 *
 * If it is a scalar, then we need to construct an npts-sized 'r'
 * that is based on the scalar value.
 * 
 * If it is 1D, then we need to coerce it to double if necessary.
 * 
 * If it is nD, then we need to create a temporary 1D array so we
 * can coerce the potentially non-contiguous 1D subsets to double.
 */
  if(ndims_r > 1) {
    r_one_d = 0;
  }
  else {
    r_one_d  = 1;
    r_scalar = is_scalar(ndims_r,dsizes_r);
  }

/*
 * Here are the three possible scenarios for "r": 
 */
  if(r_scalar) {
    tmp_r = (double*)calloc(npts,sizeof(double));
    coerce_subset_input_double(r,&tmp_r[0],0,type_r,1,0,NULL,NULL);
/*
 * Copy this scalar npts-1 times to rest of the array.
 */
    for(i = 1; i < npts; i++ ) tmp_r[i] = tmp_r[i-1] + tmp_r[0];
  }
  else if(r_one_d) {
    tmp_r = coerce_input_double(r,type_r,npts,0,NULL,NULL);
  }
  else {
    tmp_r = (double*)calloc(npts,sizeof(double));
  }
  if( tmp_r == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: Unable to allocate memory for coercing r to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_q == NCL_double || type_r == NCL_double) {
    type_dqdr    = NCL_double;
    dqdr         = (void*)calloc(size_q,sizeof(double));
    missing_dqdr = missing_dq;
  }
  else {
    type_dqdr    = NCL_float;
    dqdr         = (void*)calloc(size_q,sizeof(float));
    missing_dqdr = missing_rq;
  }
  tmp_dqdr = (double*)calloc(npts,sizeof(double));
  if( dqdr == NULL || tmp_dqdr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }


/*
 * Loop through dimensions and call Fortran routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    index_nrnpts = i*size_rightmost * npts;
    for( j = 0; j < size_rightmost; j++ ) {
      index_q = index_nrnpts + j;
/*
 * Coerce q (tmp_q) to double.
 */
      coerce_subset_input_double_step(q,tmp_q,index_q,size_rightmost,
                                      type_q,npts,0,NULL,NULL);
      if(!r_one_d) {
/*
 * Coerce r (tmp_r) to double.
 */
        coerce_subset_input_double_step(r,tmp_r,index_q,size_rightmost,
                                        type_r,npts,0,NULL,NULL);
      }
/*
 * Call the Fortran routine.
 */
      NGCALLF(dcfindif,DCFINDIF)(tmp_q,tmp_r,&inpts,&missing_dq.doubleval,
                                 &missing_dr.doubleval,cyclic,&iend,
                                 qq,rr,&inpts1,tmp_dqdr,&ier);

      coerce_output_float_or_double_step(dqdr,tmp_dqdr,type_dqdr,npts,index_q,
                                         size_rightmost);
    }
  }
/*
 * Free temp arrays.
 */
  if(type_r != NCL_double || r_scalar || !r_one_d) NclFree(tmp_r);
  NclFree(tmp_q);
  NclFree(tmp_dqdr);
  NclFree(qq);
  NclFree(rr);

  if(has_missing_q) {
    return(NclReturnValue(dqdr,ndims_q,dsizes_q,&missing_dqdr,type_dqdr,0));
  }
  else {
    return(NclReturnValue(dqdr,ndims_q,dsizes_q,NULL,type_dqdr,0));
  }
}


NhlErrorTypes uv2vr_cfd_W( void )
{
/*
 * Input array variables
 */
  void *u, *v, *lat, *lon;
  int *bound_opt;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  double *tmp_lat, *tmp_lon;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  NclScalar missing_u, missing_du, missing_ru;
  NclBasicDataTypes type_u, type_v, type_lat, type_lon;
/*
 * Output array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  NclBasicDataTypes type_vort;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, nlon, nlat, nlatnlon, size_uv, size_leftmost, index_uv;
  int ier, inlat, inlon;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  u = (void*)NclGetArgValue(
          0,
          5,
          &ndims_u,
          dsizes_u,
          &missing_u,
          &has_missing_u,
          &type_u,
          DONT_CARE);

  v = (void*)NclGetArgValue(
          1,
          5,
          &ndims_v,
          dsizes_v,
          NULL,
          NULL,
          &type_v,
          DONT_CARE);

  lat = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

  bound_opt = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Get size of input array.
 */
  if(ndims_u < 2 || ndims_u != ndims_v) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: u and v must have the same numer of dimensions and have at least 2 dimensions");
    return(NhlFATAL);
  }
  for( i=0; i < ndims_u; i++ ) {
    if(dsizes_u[i] != dsizes_v[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: u and v must have the same dimensions");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
  nlatnlon = nlat * nlon;

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

  if(dsizes_lat[0] != nlat || dsizes_lon[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: the lat,lon arrays must be dimensioned nlat and nlon, the last two dimensions of u and v");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the q array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_u-2; i++ ) size_leftmost *= dsizes_u[i];
  size_uv = size_leftmost * nlatnlon;

/*
 * Check for missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
/*
 * Create temporary arrays to hold double precision data.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_u == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: Unable to allocate memory for coercing u to double precision");
      return(NhlFATAL);
    }
  }

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_v == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: Unable to allocate memory for coercing v to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_u == NCL_double || type_v == NCL_double) {
    type_vort = NCL_double;
    vort      = (void*)calloc(size_uv,sizeof(double));
  }
  else {
    tmp_vort  = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_vort == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
    type_vort = NCL_float;
    vort      = (void*)calloc(size_uv,sizeof(float));
  }
  if(vort == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce lat/lon arrays to double if necessary.
 */
  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  tmp_lon = coerce_input_double(lon,type_lon,nlon,0,NULL,NULL);
  if(tmp_lat == NULL || tmp_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vr_cfd: Unable to coerce lat/lon arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_uv = 0;
  for(i = 0; i < size_leftmost; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_u to u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_v to v.
 */
      tmp_v = &((double*)v)[index_uv];
    }

    if(type_vort == NCL_double) {
/*
 * Point tmp_vort to vort.
 */
      tmp_vort = &((double*)vort)[index_uv];
    }

/*
 * Call the Fortran routine.
 */
    NGCALLF(dvrfidf,DVRFIDF)(tmp_u,tmp_v,tmp_lat,tmp_lon,&inlon,&inlat,
                             &missing_du.doubleval,bound_opt,tmp_vort,&ier);

    if(type_vort != NCL_double) {
      coerce_output_float_only(vort,tmp_vort,nlatnlon,index_uv);
    }
    index_uv += nlatnlon;
  }
/*
 * Free temp arrays.
 */
  if(type_u   != NCL_double) NclFree(tmp_u);
  if(type_v   != NCL_double) NclFree(tmp_v);
  if(type_lat != NCL_double) NclFree(tmp_lat);
  if(type_lon != NCL_double) NclFree(tmp_lon);
  if(type_vort!= NCL_double) NclFree(tmp_vort);

  if(type_vort == NCL_double) {
    return(NclReturnValue(vort,ndims_u,dsizes_u,&missing_du,type_vort,0));
  }
  else {
    return(NclReturnValue(vort,ndims_u,dsizes_u,&missing_ru,type_vort,0));
  }
}


NhlErrorTypes uv2dv_cfd_W( void )
{
/*
 * Input array variables
 */
  void *u, *v, *lat, *lon;
  int *bound_opt;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  double *tmp_lat, *tmp_lon;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lat[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_lon[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  NclScalar missing_u, missing_du, missing_ru;
  NclBasicDataTypes type_u, type_v, type_lat, type_lon;
/*
 * Output array variables
 */
  void *div;
  double *tmp_div = NULL;
  NclBasicDataTypes type_div;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, nlon, nlat, nlatnlon, size_uv, size_leftmost, index_uv;
  int inlat, inlon, ier;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  u = (void*)NclGetArgValue(
          0,
          5,
          &ndims_u,
          dsizes_u,
          &missing_u,
          &has_missing_u,
          &type_u,
          DONT_CARE);

  v = (void*)NclGetArgValue(
          1,
          5,
          &ndims_v,
          dsizes_v,
          NULL,
          NULL,
          &type_v,
          DONT_CARE);

  lat = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          DONT_CARE);

  lon = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          DONT_CARE);

  bound_opt = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Get size of input array.
 */
  if(ndims_u < 2 || ndims_u != ndims_v) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: u and v must have the same numer of dimensions and have at least 2 dimensions");
    return(NhlFATAL);
  }
  for( i=0; i < ndims_u; i++ ) {
    if(dsizes_u[i] != dsizes_v[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: u and v must have the same dimensions");
      return(NhlFATAL);
    }
  }
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
  nlatnlon = nlat * nlon;

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;


  if(dsizes_lat[0] != nlat || dsizes_lon[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: the lat,lon arrays must be dimensioned nlat and nlon, the last two dimensions of u and v");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the q array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_u-2; i++ ) size_leftmost *= dsizes_u[i];
  size_uv = size_leftmost * nlatnlon;

/*
 * Check for missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
/*
 * Create temporary arrays to hold double precision data.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_u == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: Unable to allocate memory for coercing u to double precision");
      return(NhlFATAL);
    }
  }

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if( tmp_v == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: Unable to allocate memory for coercing v to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array.
 */
  if(type_u == NCL_double || type_v == NCL_double) {
    type_div = NCL_double;
    div      = (void*)calloc(size_uv,sizeof(double));
  }
  else {
    tmp_div  = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_div == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
    type_div = NCL_float;
    div      = (void*)calloc(size_uv,sizeof(float));
  }
  if(div == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce lat/lon arrays to double if necessary.
 */
  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  tmp_lon = coerce_input_double(lon,type_lon,nlon,0,NULL,NULL);
  if(tmp_lat == NULL || tmp_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dv_cfd: Unable to coerce lat/lon arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_uv = 0;
  for(i = 0; i < size_leftmost; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_u to u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_v to v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_div == NCL_double) {
/*
 * Point tmp_div to div.
 */
      tmp_div = &((double*)div)[index_uv];
    }
/*
 * Call the Fortran routine.
 */
    NGCALLF(ddvfidf,DDVFIDF)(tmp_u,tmp_v,tmp_lat,tmp_lon,&inlon,&inlat,
                             &missing_du.doubleval,bound_opt,tmp_div,&ier);

    if(type_div != NCL_double) {
      coerce_output_float_only(div,tmp_div,nlatnlon,index_uv);
    }
    index_uv += nlatnlon;
  }
/*
 * Free temp arrays.
 */
  if(type_u   != NCL_double) NclFree(tmp_u);
  if(type_v   != NCL_double) NclFree(tmp_v);
  if(type_lat != NCL_double) NclFree(tmp_lat);
  if(type_lon != NCL_double) NclFree(tmp_lon);
  if(type_div != NCL_double) NclFree(tmp_div);

  if(type_div == NCL_double) {
    return(NclReturnValue(div,ndims_u,dsizes_u,&missing_du,type_div,0));
  }
  else {
    return(NclReturnValue(div,ndims_u,dsizes_u,&missing_ru,type_div,0));
  }
}

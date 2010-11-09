#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dstdatmz,DSTDATMZ)(int *, double *, double *, double *,
                                       double *);

extern void NGCALLF(dstdatmp,DSTDATMP)(int *, double *, double *, double *,
                                       double *);


NhlErrorTypes stdatmus_z2tdp_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  ng_size_t size_z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_z;

/*
 * Output variable.
 */
  void *tdp;
  double *tmp_t = NULL;
  double *tmp_d = NULL;
  double *tmp_p = NULL;
  ng_size_t size_tdp;
  int ndims_tdp;
  ng_size_t *dsizes_tdp;
  NclBasicDataTypes type_tdp;
/*
 * Various
 */
  ng_size_t i, nz, size_leftmost, index_z, index_t, index_d, index_p;
  int is_scalar_z, ret, inz;

/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           1,
           &ndims_z,
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           DONT_CARE);
/*
 * Calculate size of leftmost dimensions, and set dimension sizes for 
 * output.
 */
  is_scalar_z = is_scalar(ndims_z,dsizes_z);
  if(is_scalar_z) {
    ndims_tdp = ndims_z;         /* tdp will be of length 3 */
  }
  else  {
    ndims_tdp = ndims_z + 1;      /* tdp will be 3 x dimsizes(z) */
  }
  dsizes_tdp = (ng_size_t *)malloc(ndims_tdp*sizeof(ng_size_t));
  if(dsizes_tdp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_z2tdp: Unable to allocate memory for dimension size array");
    return(NhlFATAL);
  }

/*
 * Calculate size of leftmost dimensions, and fill in dimension sizes
 * for output array, tdp.
 */
  size_leftmost = 1;
  nz = dsizes_z[ndims_z-1];
  dsizes_tdp[0] = 3;
  if(!is_scalar_z) {
    for(i = 0; i < ndims_z; i++) {
      dsizes_tdp[i+1] = dsizes_z[i];
      if(i < (ndims_z-1)) size_leftmost *= dsizes_z[i];
    }
  }
  size_z   = size_leftmost * nz;
  size_tdp = 3 * size_z;

/*
 * Test input dimension sizes.
 */
  if(nz > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_z2tdp: nz = %ld is greater than INT_MAX",nz);
  }
  inz = (int) nz;

/* 
 * Allocate space for output arrays.  If the input z is already double,
 * then we don't need to allocate space for temporary arrays, because
 * we'll just change the pointer into the void array appropriately.
 */
  if(type_z == NCL_double) {
    type_tdp = NCL_double;
    tdp = (double *)calloc(size_tdp,sizeof(double));
    if(tdp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_z2tdp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_tdp = NCL_float;
    tdp   = (float *)calloc(size_tdp,sizeof(float));
    tmp_z = (double *)calloc(nz,sizeof(double));
    tmp_t = (double *)calloc(nz,sizeof(double));
    tmp_d = (double *)calloc(nz,sizeof(double));
    tmp_p = (double *)calloc(nz,sizeof(double));
    if(tmp_z == NULL || tdp == NULL || tmp_t == NULL || tmp_d == NULL ||
       tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_z2tdp: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Loop across leftmost dimensions and call Fortran routine for each
 * one-dimensional subsection.
 */
  index_z = 0;
  index_t = 0;         /* 1st index of leftmost dimension contains "t" vals. */
  index_d = size_z;    /* 2nd index of leftmost dimension contains "d" vals. */
  index_p = 2*size_z;  /* 3rd index of leftmost dimension contains "p" vals. */

  for(i = 0; i < size_leftmost; i++) {
    if(type_z != NCL_double) {
/*
 * Coerce subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,nz,0,NULL,NULL);
    }
    else {
/*
 * Point temporary arrays to appropriate location in z and tdp.
 */
      tmp_z = &((double*)z)[index_z];
      tmp_t = &((double*)tdp)[index_t];
      tmp_d = &((double*)tdp)[index_d];
      tmp_p = &((double*)tdp)[index_p];
    }

    NGCALLF(dstdatmz,DSTDATMZ)(&inz,tmp_z,tmp_t,tmp_d,tmp_p);

    if(type_tdp == NCL_float) {
      coerce_output_float_only(tdp,tmp_t,nz,index_t);
      coerce_output_float_only(tdp,tmp_d,nz,index_d);
      coerce_output_float_only(tdp,tmp_p,nz,index_p);
    }
    index_z += nz;
    index_t += nz;
    index_d += nz;
    index_p += nz;
  }
/*
 * Free up memory.
 */
  if(type_z != NCL_double) {
    NclFree(tmp_z);
    NclFree(tmp_t);
    NclFree(tmp_d);
    NclFree(tmp_p);
  }

  ret = NclReturnValue(tdp,ndims_tdp,dsizes_tdp,NULL,type_tdp,0);
  NclFree(dsizes_tdp);
  return(ret);
}



NhlErrorTypes stdatmus_p2tdz_W( void )
{
/*
 * Input array variables
 */
  void *p;
  double *tmp_p = NULL;
  ng_size_t size_p;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p;

/*
 * Output variable.
 */
  void *tdz;
  double *tmp_t = NULL;
  double *tmp_d = NULL;
  double *tmp_z = NULL;
  ng_size_t size_tdz;
  int ndims_tdz;
  ng_size_t *dsizes_tdz;
  NclBasicDataTypes type_tdz;
/*
 * Various
 */
  ng_size_t i, np, size_leftmost, index_p, index_t, index_d, index_z;
  int is_scalar_p, ret, inp;

/*
 * Retrieve parameter.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
           0,
           1,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           DONT_CARE);
/*
 * Calculate size of leftmost dimensions, and set dimension sizes for 
 * output.
 */
  is_scalar_p = is_scalar(ndims_p,dsizes_p);
  if(is_scalar_p) {
    ndims_tdz = ndims_p;         /* tdz will be of length 3 */
  }
  else  {
    ndims_tdz = ndims_p + 1;      /* tdz will be 3 x dimsizes(p) */
  }
  dsizes_tdz = (ng_size_t *)malloc(ndims_tdz*sizeof(ng_size_t));
  if(dsizes_tdz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_p2tdz: Unable to allocate memory for dimension size array");
    return(NhlFATAL);
  }

/*
 * Calculate size of leftmost dimensions, and fill in dimension sizes
 * for output array, tdz.
 */
  size_leftmost = 1;
  np = dsizes_p[ndims_p-1];
  dsizes_tdz[0] = 3;
  if(!is_scalar_p) {
    for(i = 0; i < ndims_p; i++) {
      dsizes_tdz[i+1] = dsizes_p[i];
      if(i < (ndims_p-1)) size_leftmost *= dsizes_p[i];
    }
  }
  size_p   = size_leftmost * np;
  size_tdz = 3 * size_p;

/*
 * Test input dimension sizes.
 */
  if(np > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_p2tdz: np = %ld is greater than INT_MAX",np);
     return(NhlFATAL);
  }
  inp = (int) np;

/* 
 * Allocate space for output arrays.  If the input p is already double,
 * then we don't need to allocate space for temporary arrays, because
 * we'll just change the pointer into the void array appropriately.
 */
  if(type_p == NCL_double) {
    type_tdz = NCL_double;
    tdz = (double *)calloc(size_tdz,sizeof(double));
    if(tdz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_p2tdz: Unable to allocate memory for output array"); 
     return(NhlFATAL);
    }
  }
  else {
    type_tdz = NCL_float;
    tdz   = (float *)calloc(size_tdz,sizeof(float));
    tmp_p = (double *)calloc(np,sizeof(double));
    tmp_t = (double *)calloc(np,sizeof(double));
    tmp_d = (double *)calloc(np,sizeof(double));
    tmp_z = (double *)calloc(np,sizeof(double));
    if(tmp_p == NULL || tdz == NULL || tmp_t == NULL || tmp_d == NULL ||
       tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stdatmus_p2tdz: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Loop across leftmost dimensions and call Fortran routine for each
 * one-dimensional subsection.
 */
  index_p = 0;
  index_t = 0;         /* 1st index of leftmost dimension contains "t" vals. */
  index_d = size_p;    /* 2nd index of leftmost dimension contains "d" vals. */
  index_z = 2*size_p;  /* 3rd index of leftmost dimension contains "z" vals. */

  for(i = 0; i < size_leftmost; i++) {
    if(type_p != NCL_double) {
/*
 * Coerce subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,index_p,type_p,np,0,NULL,NULL);
    }
    else {
/*
 * Point temporary arrays to appropriate location in p and tdz.
 */
      tmp_p = &((double*)p)[index_p];
      tmp_t = &((double*)tdz)[index_t];
      tmp_d = &((double*)tdz)[index_d];
      tmp_z = &((double*)tdz)[index_z];
    }

    NGCALLF(dstdatmp,DSTDATMP)(&inp,tmp_p,tmp_t,tmp_d,tmp_z);

    if(type_tdz == NCL_float) {
      coerce_output_float_only(tdz,tmp_t,np,index_t);
      coerce_output_float_only(tdz,tmp_d,np,index_d);
      coerce_output_float_only(tdz,tmp_z,np,index_z);
    }
    index_p += np;
    index_t += np;
    index_d += np;
    index_z += np;
  }
/*
 * Free up memory.
 */
  if(type_p != NCL_double) {
    NclFree(tmp_p);
    NclFree(tmp_t);
    NclFree(tmp_d);
    NclFree(tmp_z);
  }

  ret = NclReturnValue(tdz,ndims_tdz,dsizes_tdz,NULL,type_tdz,0);
  NclFree(dsizes_tdz);
  return(ret);
}


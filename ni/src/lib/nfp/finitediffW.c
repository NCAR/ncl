#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(dcfindif,DCFINDIF)(double *,double *,int *,double *,
                                       double *,int *,int *, double *,
                                       double *,int *,double *,int *);

NhlErrorTypes center_finite_diff_W( void )
{
/*
 * Input array variables
 */
  void *q, *r;
  logical *cyclic;
  int *opt;
  double *tmp_q, *tmp_r;
  int ndims_q, dsizes_q[NCL_MAX_DIMENSIONS];
  int ndims_r, dsizes_r[NCL_MAX_DIMENSIONS];
  int has_missing_q, has_missing_r;
  NclScalar missing_q, missing_dq, missing_rq;
  NclScalar missing_r, missing_dr, missing_rr;
  NclBasicDataTypes type_q, type_r, type_dqdr;
/*
 * Output array variables
 */
  void *dqdr;
  double *tmp_dqdr;
/*
 * Declare various variables for random purposes.
 */
  int i, j, npts, npts1, size_q, size_leftmost, index_q, iend, ier;
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
          2);

  r = (void*)NclGetArgValue(
          1,
          4,
          &ndims_r,
          dsizes_r,
          &missing_r,
          &has_missing_r,
          &type_r,
          2);

  cyclic = (logical*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  opt = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Get size of input array.
 */
  npts  = dsizes_q[ndims_q-1];
  npts1 = npts + 1;
  if(dsizes_r[0] > 1 && dsizes_r[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: r must either be a scalar or a 1D array the same length as the rightmost dimemsion of q");
    return(NhlFATAL);
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
  coerce_missing(type_r,has_missing_r,&missing_r,&missing_dr,&missing_rr);
/*
 * Create arrays to hold temporary r and q values.
 */
  tmp_q = (double*)calloc(npts,sizeof(double));
  tmp_r = (double*)calloc(npts,sizeof(double));
  qq    = (double*)calloc(npts+2,sizeof(double));
  rr    = (double*)calloc(npts+2,sizeof(double));
  if( tmp_q == NULL || tmp_r == NULL || qq == NULL || rr == NULL) {
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

  if(type_r != NCL_double || dsizes_r[0] == 1) {
    tmp_r = (double*)calloc(npts,sizeof(double));
    if( tmp_r == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for coercing r to double precision");
      return(NhlFATAL);
    }
  }

  if(type_r != NCL_double || dsizes_r[0] == 1) {
/*
 * Coerce r (tmp_r) to double.
 */
    coerce_subset_input_double(r,tmp_r,0,type_r,dsizes_r[0],0,NULL,NULL);
/*
 * If r is a scalar, then copy it npts-1 times to rest of the array.
 */
    if(dsizes_r[0] == 1) {
      for(i = 1; i < npts; i++ ) tmp_r[i] = tmp_r[i-1] + tmp_r[0];
    }
  }
  else {
/*
 * Point tmp_r to r.
 */
    tmp_r = &((double*)r)[0];
  }
/*
 * Allocate space for output array.
 */
  tmp_dqdr = (double*)calloc(npts,sizeof(double));
  if(type_q == NCL_double || type_r == NCL_double) {
    type_dqdr = NCL_double;
    dqdr      = (void*)calloc(size_q,sizeof(double));
  }
  else {
    type_dqdr = NCL_float;
    dqdr      = (void*)calloc(size_q,sizeof(float));
  }
  if( dqdr == NULL || tmp_dqdr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"center_finite_diff: Unable to allocate memory for output array");
    return(NhlFATAL);
  }


/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  if(*cyclic) {
    iend = 0;
  }
  else {
    iend = 1;
  }

  index_q = 0;
  for(i = 0; i < size_leftmost; i++ ) {
    if(type_q != NCL_double) {
/*
 * Coerce delta (tmp_q) to double.
 */
      coerce_subset_input_double(q,tmp_q,index_q,type_q,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to q.
 */
      tmp_q = &((double*)q)[index_q];
  }
/*
 * Call the Fortran routine.
 */
    NGCALLF(dcfindif,DCFINDIF)(tmp_q,tmp_r,&npts,&missing_dq.doubleval,
                               &missing_dr.doubleval,cyclic,&iend,
                               qq,rr,&npts1,tmp_dqdr,&ier);
    for(j = 0; j < npts; j++) {
      if(type_dqdr == NCL_double) {
        ((double*)dqdr)[index_q+j] = tmp_dqdr[j];
      }
      else {
        ((float*)dqdr)[index_q+j] = (float)(tmp_dqdr[j]);
      }
    }

    index_q += npts;
  }
/*
 * Free temp arrays.
 */
  if(type_r != NCL_double || dsizes_r[0] == 1) NclFree(tmp_r);
  if(type_q != NCL_double) NclFree(tmp_q);
  NclFree(tmp_dqdr);
  NclFree(qq);
  NclFree(rr);

  if(has_missing_q) {
    if(type_q == NCL_double) {
      return(NclReturnValue(dqdr,ndims_q,dsizes_q,&missing_dq,type_dqdr,0));
    }
    else {
      return(NclReturnValue(dqdr,ndims_q,dsizes_q,&missing_rq,type_dqdr,0));
    }
  }
  else {
    return(NclReturnValue(dqdr,ndims_q,dsizes_q,NULL,type_dqdr,0));
  }
}

#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"

extern int NGCALLF(ilaenv,ILAENV)(int*,char*,char*,int*,int*,int*,int*,
                                  int,int);
extern void NGCALLF(dgetrf,DGETRF)(int*, int*, double*, int*, int*, int*);
extern void NGCALLF(dgetri,DGETRI)(int*, double*, int*, int*, double*, 
                                   int*, int*);
extern void NGCALLF(dgesv,DGESV)(int*, int*, double*, int*, int*, double*,
                                 int*, int*);

NhlErrorTypes inverse_matrix_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  ng_size_t dsizes_x[2];
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xinv;
  double xmsg;
  NclBasicDataTypes type_xinv;
  NclScalar missing_xinv;
/*
 * Various
 */
  ng_size_t n, m, nm, lwork, lpiv, nb, lda;
  int one, mone, in, im, inrhs, ilda, ilwork;
  int info=0, *ipiv;
  double *work;
/*
 * Retrieve input array. 
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/*
 * Test dimension sizes.
 */
  n  = dsizes_x[0];
  m  = dsizes_x[1];
  nm = n * m;

  if((n > INT_MAX) || (m > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"inverse_matrix: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  in = (int) n;
  im = (int) m;

/*
 * Coerce input matrix to double no matter what, since it will get
 * changed by Fortran routine.
 */
  tmp_x = (double*)calloc(nm,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"inverse_matrix: Unable to allocate memory for making a copy of the input array");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_x == NCL_double) {
    xinv = (void*)calloc(nm,sizeof(double));
    type_xinv = NCL_double;
    missing_xinv.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    xmsg = missing_xinv.doubleval;
  }
  else {
    xinv = (void*)calloc(nm,sizeof(float));
    type_xinv = NCL_float;
    missing_xinv.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    xmsg = (double)missing_xinv.floatval;
  }
  if(xinv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"inverse_matrix: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for work arrays.
 */
  mone  = -1;
  one   =  1;
  lda   = m;

  nb = NGCALLF(ilaenv,ILAENV)(&one, "dgetri", " ", &in, &mone, &mone, 
                              &mone, 6, 1);

  lwork = n * nb;
  lpiv  = min(n,m);
  if((lwork > INT_MAX) || (lda > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"inverse_matrix: one or more work array dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  ilda = (int) lda;
  ilwork = (int) lwork;

  work  = (double*)calloc(lwork,sizeof(double));
  ipiv  = (int*)calloc(lpiv,sizeof(int));
  if(work == NULL || ipiv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"inverse_matrix: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Coerce subsection of x (tmp_x) to double no matter what, because the
 * inverse routine will write over the input array.
 */
  coerce_subset_input_double(x,tmp_x,0,type_x,nm,0,NULL,NULL);

  NGCALLF(dgetrf,DGETRF)( &im, &in, tmp_x, &ilda, ipiv, &info );
  if(!info) {
    NGCALLF(dgetri,DGETRI)( &in, tmp_x, &ilda, ipiv, work, &ilwork, &info );
    if(!info) {
      coerce_output_float_or_double(xinv,tmp_x,type_xinv,nm,0);
    }
  }
  else {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"inverse_matrix: info = %d; missing values not allowed\n", info );
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(work);
  NclFree(ipiv);
/*
 * Return.
 */
  if(info) {
    set_subset_output_missing(xinv,0,type_xinv,nm,xmsg);
    return(NclReturnValue(xinv,2,dsizes_x,&missing_xinv,type_xinv,0));
  }
  else {
    return(NclReturnValue(xinv,2,dsizes_x,NULL,type_xinv,0));
  }
}



NhlErrorTypes solve_linsys_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *tmp_a, *tmp_b, *tmp_tmp_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *x;
  NclBasicDataTypes type_x;
  NclScalar missing_x;
  double xmsg;
/*
 * Various
 */
  ng_size_t i, j, l, n, nrhs, nn, nnrhs;
  ng_size_t lda, ldb;
  int info=0;
  int *ipiv, in, inrhs, ilda, ildb;
/*
 * Retrieve input arrays. 
 */
  a = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           DONT_CARE);

  b = (void*)NclGetArgValue(
           1,
           2,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           DONT_CARE);

  n  = dsizes_a[0];
  if(ndims_b > 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_lynsys: The first input array must be two-dimensionsal, and the second array must be one or two-dimensional");
    return(NhlFATAL);
  }

  if(dsizes_b[ndims_b-1] != n || dsizes_a[1] != n) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_linsys: The two dimensions of 'a' and the rightmost dimension of 'b' must be n");
    return(NhlFATAL);
  }

  if(ndims_b==2) {
    nrhs = dsizes_b[0];
  }
  else {
    nrhs = 1;
  }

  nn    = n * n;
  nnrhs = n * nrhs;
  lda   = ldb = n;

  if((n > INT_MAX) || (nrhs > INT_MAX) || (lda > INT_MAX) || (ldb > INT_MAX)){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_linsys: one or more dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  in = (int) n;
  inrhs = (int) nrhs;
  ilda = (int) lda;
  ildb = (int) ldb;

/*
 * Coerce input matrix to double no matter what, since it will get
 * changed by Fortran routine.
 */
  tmp_a = (double*)calloc(nn,sizeof(double));
  tmp_b = (double*)calloc(nnrhs,sizeof(double));
  tmp_tmp_a = (double*)calloc(nn,sizeof(double));
  if( tmp_a == NULL || tmp_b == NULL || tmp_tmp_a == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_linsys: Unable to allocate memory for making a copy of the input arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  if(type_a == NCL_double || type_b == NCL_double) {
    x = (void*)calloc(nnrhs,sizeof(double));
    type_x = NCL_double;
    missing_x.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    xmsg = missing_x.doubleval;
  }
  else {
    x = (void*)calloc(nnrhs,sizeof(float));
    type_x = NCL_float;
    missing_x.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    xmsg = (double)missing_x.floatval;
  }
  if(x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_linsys: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for work arrays.
 */
  ipiv = (int*)calloc(n,sizeof(int));
  if(ipiv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"solve_linsys: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Coerce subsection of input to double no matter what, because the
 * inverse routine will write over the input arrays.  We also need to
 * transpose a.
 */
  coerce_subset_input_double(a,tmp_tmp_a,0,type_a,nn,0,NULL,NULL);
  coerce_subset_input_double(b,tmp_b,0,type_b,nnrhs,0,NULL,NULL);

  l = 0;
  for(i = 0; i < n; i++ ) {
    for(j = 0; j < n; j++ ) {
      tmp_a[l++] = tmp_tmp_a[j*n+i];
    }
  }

  NGCALLF(dgesv,DGESV)( &in, &inrhs, tmp_a, &ilda, ipiv, tmp_b, &ildb, &info );

  if(!info) {
    coerce_output_float_or_double(x,tmp_b,type_x,nnrhs,0);
  }
  else {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"solve_linsys: info = %d, missing values returned\n", info );
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_a);
  NclFree(tmp_tmp_a);
  NclFree(tmp_b);
  NclFree(ipiv);
/*
 * Return.
 */
  if(info) {
    set_subset_output_missing(x,0,type_x,nnrhs,xmsg);
    return(NclReturnValue(x,ndims_b,dsizes_b,&missing_x,type_x,0));
  }
  else {
    return(NclReturnValue(x,ndims_b,dsizes_b,NULL,type_x,0));
  }
}



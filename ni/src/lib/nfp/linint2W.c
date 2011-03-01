#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dlinint1,DLININT1)(int *,double *,double *,int *,int *,
                                       double *,double *,double *,double *,
                                       int *,double *,int *,int *);

extern void NGCALLF(dlinint2,DLININT2)(int *,double *,int *,double *,
                                       double *,int *,int *,double *,int *,
                                       double *,double *,double *, double *,
                                       int *,double *,int *,int *);


extern void NGCALLF(dlinint2pts,DLININT2PTS)(int *,double *,int *,double *,
                                             double *,int *,int *,double *,
                                             double *,double *,double *,
                                             double *,int *,double *,int *);

extern void NGCALLF(arealinint2da,AREALININT2DA)(
                                      int*, int*, int*, double*, double*,
                                      double*, double*, double*, double*,
                                      int*, int*, int*, int*, double*, 
                                      double*, double*, double*, int*, int*,
                                      double*, double*, double*, double*, 
                                      double *, double*, double*, double*,
                                      double*, double*, double*, double*,
                                      double*, int*, int*);

NhlErrorTypes linint1_W( void )
{
/*
 * Input variables
 */
  void *xi, *fi, *xo;
  double *tmp_xi = NULL;
  double *tmp_fi = NULL;
  double *tmp_xo, *tmp_fo;
  int ndims_xi;
  int ndims_fi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi;
  ng_size_t *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi, missing_fo;
  int *opt, iopt = 0;
  logical *wrap;
  NclBasicDataTypes type_xi, type_fi, type_xo, type_fo;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  ng_size_t nxi, nxi2, nxo, nfo, size_leftmost, size_fo;
  int inxi, inxi2, inxo, ier, ret;
  ng_size_t i, j, index_xi, index_fi, index_fo;
  double *xiw, *fxiw;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          1,
          5,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  wrap = (logical*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  xo = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Compute the total number of elements in our arrays and check them.
 */
  nxi  = dsizes_xi[ndims_xi-1];
  nxo  = dsizes_xo[0];
  nfo  = nxo;
  nxi2 = nxi + 2;

  if(nxi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: xi must have at least 2 elements");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nxi > INT_MAX) || (nxo > INT_MAX) || (nxi2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inxi  = (int) nxi;
  inxo  = (int) nxo;
  inxi2 = (int) nxi2;

/*
 * Check dimensions of xi and fi. If xi is not one-dimensional, then it 
 * must be the same size as fi. Otherwise, the rightmost dimension of
 * fi must be equal to the length of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_fi) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: If xi is not one-dimensional, then it must be the same size as fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_fi; i++) {
      if(dsizes_xi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: If xi is not one-dimensional, then it must be the same size as fi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_fi[ndims_fi-1] != nxi) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: The rightmost dimension of fi must be the same length as xi");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the output array (minus the last dimension).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_fi-1; i++ ) size_leftmost *= dsizes_fi[i];
  size_fo = size_leftmost * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nfo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (ng_size_t*)calloc(ndims_fi,sizeof(ng_size_t));
  if(type_fi == NCL_double) {
    fo         = (void*)calloc(size_fo,sizeof(double));
    type_fo    = NCL_double;
    missing_fo = missing_dfi;
  }
  else {
    fo         = (void*)calloc(size_fo,sizeof(float));
    type_fo    = NCL_float;
    missing_fo = missing_rfi;
  }
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-1; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-1] = nxo;

/*
 * Allocate space for work arrays.
 */
  xiw  = (double*)calloc(nxi2,sizeof(double));
  fxiw = (double*)calloc(nxi2,sizeof(double));
  if(xiw == NULL || fxiw == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Coerce output array to double if necessary.
 */
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  if(tmp_xo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to coerce output array to double precision");
    return(NhlFATAL);
  }

  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(nxi,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_fi != NCL_double) {
    tmp_fi = (double*)calloc(nxi,sizeof(double));
    if(tmp_fi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran function.
 */
  index_xi = index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_xi > 1 || i == 0) {
      if(type_xi != NCL_double) { 
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,nxi,0,NULL,NULL);
      }
      else {
        tmp_xi = &((double*)xi)[index_xi];
      }
    }
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nxi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(dlinint1,DLININT1)(&inxi,tmp_xi,tmp_fi,wrap,&inxo,tmp_xo,tmp_fo,
                               xiw,fxiw,&inxi2,&missing_dfi.doubleval,
                               &iopt,&ier);

    if(ier) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"linint1: xi and xo must be monotonically increasing");
      for(j = 0; j < nfo; j++) {
        if(type_fi == NCL_double) {
          ((double*)fo)[index_fo+j] = missing_dfi.doubleval;
        }
        else {
          ((float*)fo)[index_fo+j] = missing_rfi.floatval;
        }
      }
    }
    else {
      coerce_output_float_or_double(fo,tmp_fo,type_fi,nfo,index_fo);
    }
    if(ndims_xi > 1) index_xi += nxi;
    index_fi += nxi;
    index_fo += nfo;
  }
/*
 * Free temp arrays.
 */
  if(type_xi != NCL_double) NclFree(tmp_xi);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_fi != NCL_double) NclFree(tmp_fi);
  NclFree(tmp_fo);
  NclFree(xiw);
  NclFree(fxiw);

  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_fo,type_fo,0);
  NclFree(dsizes_fo);
  return(ret);
}

NhlErrorTypes linint1_n_W( void )
{
/*
 * Input variables
 */
  void *xi, *fi, *xo;
  double *tmp_xi, *tmp_xo,*tmp_fi, *tmp_fo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS], dsizes_xo[NCL_MAX_DIMENSIONS];
  int ndims_fi;
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi;
  ng_size_t *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi, missing_fo;
  int *dim, *opt, iopt = 0;
  logical *wrap;
  NclBasicDataTypes type_xi, type_fi, type_xo, type_fo;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  ng_size_t nxi, nxi2, nxo, nfo, nd, nr, nl, nrnxi, nrnxo, ntotal, size_fo;
  int inxi, inxi2, inxo, ier, ret;
  ng_size_t i, j, index_nri, index_nro, index_fi, index_fo;
  double *xiw, *fxiw;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          6,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          1,
          6,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  wrap = (logical*)NclGetArgValue(
          2,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  xo = (void*)NclGetArgValue(
          3,
          6,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          4,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  dim = (int*)NclGetArgValue(
          5,
          6,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Some error checking. Make sure input dimension is valid.
 */
  if(*dim < 0 || *dim >= ndims_fi) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Invalid dimension to do interpolation on, can't continue");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our arrays and check them.
 */
  nxi  = dsizes_fi[*dim];
  nxo  = dsizes_xo[0];
  nfo  = nxo;
  nxi2 = nxi + 2;

  if(nxi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: xi must have at least 2 elements");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nxi > INT_MAX) || (nxo > INT_MAX) || (nxi2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inxi  = (int) nxi;
  inxo  = (int) nxo;
  inxi2 = (int) nxi2;

/*
 * Check dimensions of xi and fi. If xi is not one-dimensional, then it 
 * must be the same size as fi. Otherwise, the dims-th dimension of
 * fi must be equal to the length of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_fi) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: If xi is not one-dimensional, then it must be the same size as fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_fi; i++) {
      if(dsizes_xi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: If xi is not one-dimensional, then it must be the same size as fi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_xi[0] != nxi) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: The dim-th dimension of fi must be the same length as xi");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of leftmost dimensions (nl) up to the dim-th
 *   dimension.
 * Calculate size of rightmost dimensions (nr) from the
 *   dim-th dimension.
 *
 * The dimension to do the interpolation across is "dim".
 */
  nl = nr = 1;
  if(ndims_fi > 1) {
    nd = ndims_fi-1;
    for(i = 0; i < *dim ; i++) {
      nl = nl*dsizes_fi[i];
    }
    for(i = *dim+1; i < ndims_fi; i++) {
      nr = nr*dsizes_fi[i];
    }
  }
  else {
    nd = 1;
  }
  ntotal  = nr * nl;
  size_fo = ntotal * nfo;

/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nfo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (ng_size_t*)calloc(ndims_fi,sizeof(ng_size_t));
  if(type_fi == NCL_double) {
    fo         = (void*)calloc(size_fo,sizeof(double));
    type_fo    = NCL_double;
    missing_fo = missing_dfi;
  }
  else {
    fo         = (void*)calloc(size_fo,sizeof(float));
    type_fo    = NCL_float;
    missing_fo = missing_rfi;
  }
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/* 
 * Go ahead and copy all dimesions, but then replace the dim-th one.
 */
  for(i = 0; i < ndims_fi; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[*dim] = nxo;

/*
 * Allocate space for work arrays.
 */
  xiw  = (double*)calloc(nxi2,sizeof(double));
  fxiw = (double*)calloc(nxi2,sizeof(double));
  if(xiw == NULL || fxiw == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Coerce output array to double if necessary.
 */
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  if(tmp_xo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to coerce output array to double precision");
    return(NhlFATAL);
  }

  if(ndims_xi == 1) {
    tmp_xi = coerce_input_double(xi,type_xi,nxi,0,NULL,NULL);
  }
  else {
    tmp_xi = (double*)calloc(nxi,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

  tmp_fi = (double*)calloc(nxi,sizeof(double));
  if(tmp_fi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint1_n: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }

/*
 * Loop through leftmost and rightmost dimensions and call Fortran
 * routine for each array subsection.
 */
  nrnxi = nr*nxi;
  nrnxo = nr*nxo;
  for( i = 0; i < nl; i++ ) {
    index_nri = i*nrnxi;
    index_nro = i*nrnxo;
    for( j = 0; j < nr; j++ ) {
      index_fi = index_nri+j;
      index_fo = index_nro+j;

      if(ndims_xi > 1) {
        coerce_subset_input_double_step(xi,tmp_xi,index_fi,nr,type_xi,
                                        nxi,0,NULL,NULL);
      }
      coerce_subset_input_double_step(fi,tmp_fi,index_fi,nr,type_fi,
                                      nxi,0,NULL,NULL);
/*
 * Call Fortran routine.
 */
      NGCALLF(dlinint1,DLININT1)(&inxi,tmp_xi,tmp_fi,wrap,&inxo,tmp_xo,tmp_fo,
                                 xiw,fxiw,&inxi2,&missing_dfi.doubleval,
                                 &iopt,&ier);

      if(ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"linint1_n: xi and xo must be monotonically increasing");
        set_subset_output_missing_step(fo,index_fo,nr,type_fo,nfo,
                                       missing_dfi.doubleval);
      }
      else {
        coerce_output_float_or_double_step(fo,tmp_fo,type_fi,nfo,index_fo,nr);
      }
    }
  }
/*
 * Free temp arrays.
 */
  if(ndims_xi > 1 || type_xi != NCL_double) NclFree(tmp_xi);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  NclFree(tmp_fi);
  NclFree(tmp_fo);
  NclFree(xiw);
  NclFree(fxiw);

  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_fo,type_fo,0);

  NclFree(dsizes_fo);
  return(ret);
}

NhlErrorTypes linint2_W( void )
{
/*
 * Input variables
 */
  void *xi, *yi, *fi, *xo, *yo;
  double *tmp_xi = NULL;
  double *tmp_yi = NULL;
  double *tmp_fi = NULL;
  double *tmp_xo, *tmp_yo, *tmp_fo;
  int ndims_xi;
  int ndims_yi;
  int ndims_fi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_yo[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi;
  ng_size_t *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  int *opt, iopt = 0;
  logical *wrap;
  NclBasicDataTypes type_xi, type_yi, type_fi, type_xo, type_yo;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  ng_size_t nxi, nyi, nxi2, nfi, nxo, nyo, nfo, size_leftmost, size_fo;
  ng_size_t i, j, index_xi, index_yi, index_fi, index_fo;
  int inxi, inyi, inxi2, inxo, inyo, ier, ret;
  double *xiw, *fxiw;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          7,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

  yi = (void*)NclGetArgValue(
          1,
          7,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          7,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  wrap = (logical*)NclGetArgValue(
          3,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  xo = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

  yo = (void*)NclGetArgValue(
          5,
          7,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Compute the total number of elements in our arrays.
 */
  nxi  = dsizes_xi[ndims_xi-1];
  nyi  = dsizes_yi[ndims_yi-1];
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nfi  = nxi * nyi;
  nfo  = nxo * nyo;
  nxi2 = nxi + 2;
  if(nxi < 2 || nyi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: xi and yi must both have at least two elements");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes.
 */
  if((nxi > INT_MAX) || (nyi > INT_MAX) || (nxo > INT_MAX) || 
     (nyo > INT_MAX) || (nxi2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inxi  = (int) nxi;
  inyi  = (int) nyi;
  inxo  = (int) nxo;
  inyo  = (int) nyo;
  inxi2 = (int) nxi2;
/*
 * Check dimensions of xi, yi, and fi. If xi/yi are not one-dimensional,
 * then their leftmost dimensions must be the same size as the leftmost
 * dimensions of fi. The last two dimensions of fi must be nyi x nxi.
 */
  if(ndims_xi > 1) { 
    if(ndims_xi != ndims_fi-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: If xi is not one-dimensional, then it must have one less dimension than fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi-1; i++) {
      if(dsizes_xi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: If xi is not one-dimensional, then its leftmost dimensions must be the same as the leftmost dimensions of fi");
        return(NhlFATAL);
      }
    }
  }
  if(ndims_yi > 1) { 
    if(ndims_yi != ndims_fi-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: If yi is not one-dimensional, then it must have one less dimension than fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_yi-1; i++) {
      if(dsizes_yi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: If yi is not one-dimensional, then its leftmost dimensions must be the same as the leftmost dimensions of fi");
        return(NhlFATAL);
      }
    }
  }
  if(dsizes_fi[ndims_fi-2] != nyi || dsizes_fi[ndims_fi-1] != nxi) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: The rightmost dimensions of fi must be nyi x nxi, where nyi and nxi are the lengths of yi and xi respectively");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last two
 * dimensions).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_fi-2; i++ ) size_leftmost *= dsizes_fi[i];
  size_fo = size_leftmost * nfo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nfo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (ng_size_t*)calloc(ndims_fi,sizeof(ng_size_t));
  if(type_fi == NCL_double) {
    fo = (void*)calloc(size_fo,sizeof(double));
  }
  else {
    fo = (void*)calloc(size_fo,sizeof(float));
  }
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nyo;
  dsizes_fo[ndims_fi-1] = nxo;
/*
 * Allocate space for work arrays.
 */
  xiw  = (double*)calloc(nxi2,sizeof(double));
  fxiw = (double*)calloc(nxi2,sizeof(double));
  if(xiw == NULL || fxiw == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Coerce input arrays to double if necessary.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(nxi,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing xi to double precision");
      return(NhlFATAL);
    }
  }

  if(type_yi != NCL_double) {
    tmp_yi = (double*)calloc(nyi,sizeof(double));
    if(tmp_yi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing yi to double precision");
      return(NhlFATAL);
    }
  }

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);
  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to coerce output arrays to double precision");
    return(NhlFATAL);
  }

  if(type_fi != NCL_double) {
    tmp_fi = (double*)calloc(nfi,sizeof(double));
    if(tmp_fi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran function.
 */
  index_xi = index_yi = index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_xi > 1 || i == 0) {
      if(type_xi != NCL_double) { 
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,nxi,0,NULL,NULL);
      }
      else {
        tmp_xi = &((double*)xi)[index_xi];
      }
    }
    if(ndims_yi > 1 || i == 0) {
      if(type_yi != NCL_double) { 
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,nyi,0,NULL,NULL);
      }
      else {
        tmp_yi = &((double*)yi)[index_yi];
      }
    }
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nfi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(dlinint2,DLININT2)(&inxi,tmp_xi,&inyi,tmp_yi,tmp_fi,wrap,&inxo,
                               tmp_xo,&inyo,tmp_yo,tmp_fo,xiw,fxiw,&inxi2,
                               &missing_dfi.doubleval,&iopt,&ier);

    if(ier) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"linint2: xi, yi, xo, and yo must be monotonically increasing");
      for(j = 0; j < nfo; j++) {
        if(type_fi == NCL_double) {
          ((double*)fo)[index_fo+j] = missing_dfi.doubleval;
        }
        else {
          ((float*)fo)[index_fo+j] = missing_rfi.floatval;
        }
      }
    }
    else {
      coerce_output_float_or_double(fo,tmp_fo,type_fi,nfo,index_fo);
    }
    if(ndims_xi > 1) index_xi += nxi;
    if(ndims_yi > 1) index_yi += nyi;
    index_fi += nfi;
    index_fo += nfo;
  }
/*
 * Free temp arrays.
 */
  if(type_xi != NCL_double) NclFree(tmp_xi);
  if(type_yi != NCL_double) NclFree(tmp_yi);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_fi != NCL_double) NclFree(tmp_fi);
  NclFree(tmp_fo);
  NclFree(xiw);
  NclFree(fxiw);

  if(type_fi == NCL_double) {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_dfi,NCL_double,0);
  }
  else {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_rfi,NCL_float,0);
  }
  NclFree(dsizes_fo);
  return(ret);
}

NhlErrorTypes linint2_points_W( void )
{
/*
 * Input variables
 */
  void *xi, *yi, *fi, *xo, *yo;
  double *tmp_xi = NULL;
  double *tmp_yi = NULL;
  double *tmp_fi = NULL;
  double *tmp_xo, *tmp_yo, *tmp_fo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS], dsizes_yo[NCL_MAX_DIMENSIONS];
  int ndims_fi;
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi;
  ng_size_t *dsizes_fo;
  NclScalar missing_fi, missing_dfi, missing_rfi;
  int *opt;
  logical *wrap;
  NclBasicDataTypes type_xi, type_yi, type_fi, type_xo, type_yo;
/*
 * Output variables.
 */
  void *fo;
/*
 * Other variables
 */
  double *xiw, *fxiw;
  ng_size_t nxi, nxi2, nyi, nfi, nxyo, size_leftmost, size_fo;
  ng_size_t i, j, index_xi, index_yi, index_fi, index_fo;
  int inxi, inxi2, inyi, inxyo, ier, ret;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          7,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

  yi = (void*)NclGetArgValue(
          1,
          7,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          7,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  wrap = (logical*)NclGetArgValue(
          3,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  xo = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

  yo = (void*)NclGetArgValue(
          5,
          7,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

  opt = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Compute the total number of elements in our arrays.
 */
  nxi  = dsizes_xi[ndims_xi-1];
  nyi  = dsizes_yi[ndims_yi-1];
  nxyo = dsizes_xo[0];
  nxi2 = nxi+2;
  if(dsizes_yo[0] != nxyo) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: xo and yo must be the same length");
    return(NhlFATAL);
  }
  if(nxi < 2 || nyi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: xi and yi must both have at least two elements");
    return(NhlFATAL);
  }
  nfi = nxi * nyi;

/*
 * Test dimension sizes.
 */
  if((nxi > INT_MAX) || (nyi > INT_MAX) || (nxyo > INT_MAX) || 
     (nxi2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inxi  = (int) nxi;
  inyi  = (int) nyi;
  inxyo = (int) nxyo;
  inxi2 = (int) nxi2;

/*
 * Check dimensions of xi, yi, and fi. If xi/yi are not one-dimensional,
 * then their leftmost dimensions must be the same size as the leftmost
 * dimensions of fi. The last two dimensions of fi must be nyi x nxi.
 */
  if(ndims_xi > 1) { 
    if(ndims_xi != ndims_fi-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: If xi is not one-dimensional, then it must have one less dimension than fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi-1; i++) {
      if(dsizes_xi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: If xi is not one-dimensional, then its leftmost dimensions must be the same as the leftmost dimensions of fi");
        return(NhlFATAL);
      }
    }
  }
  if(ndims_yi > 1) { 
    if(ndims_yi != ndims_fi-1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: If yi is not one-dimensional, then it must have one less dimension than fi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_yi-1; i++) {
      if(dsizes_yi[i] != dsizes_fi[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: If yi is not one-dimensional, then its leftmost dimensions must be the same as the leftmost dimensions of fi");
        return(NhlFATAL);
      }
    }
  }
  if(dsizes_fi[ndims_fi-2] != nyi || dsizes_fi[ndims_fi-1] != nxi) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: The rightmost dimensions of fi must be nyi x nxi, where nyi and nxi are the lengths of yi and xi respectively");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the last two dimensions).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_fi-2; i++ ) size_leftmost *= dsizes_fi[i];
  size_fo = size_leftmost * nxyo;
/*
 * Coerce missing values.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for temporary output array.
 */
  tmp_fo = (double*)calloc(nxyo,sizeof(double));
  if(tmp_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for temporary arrays");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dsizes_fo = (ng_size_t*)calloc(ndims_fi-1,sizeof(ng_size_t));
  if(type_fi == NCL_double) {
    fo = (void*)calloc(size_fo,sizeof(double));
  }
  else {
    fo = (void*)calloc(size_fo,sizeof(float));
  }
  if(fo == NULL || dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nxyo;

/*
 * Allocate space for work arrays.
 */
  xiw  = (double*)calloc(nxi2,sizeof(double));
  fxiw = (double*)calloc(nyi*nxi2,sizeof(double));
  if(xiw == NULL || fxiw == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Coerce input arrays to double if necessary.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(nxi,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for coercing xi to double precision");
      return(NhlFATAL);
    }
  }

  if(type_yi != NCL_double) {
    tmp_yi = (double*)calloc(nyi,sizeof(double));
    if(tmp_yi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for coercing yi to double precision");
      return(NhlFATAL);
    }
  }

  tmp_xo = coerce_input_double(xo,type_xo,nxyo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nxyo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

  if(type_fi != NCL_double) {
    tmp_fi = (double*)calloc(nfi,sizeof(double));
    if(tmp_fi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2_points: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran function.
 */
  index_xi = index_yi = index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_xi > 1 || i == 0) {
      if(type_xi != NCL_double) { 
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,nxi,0,NULL,NULL);
      }
      else {
        tmp_xi = &((double*)xi)[index_xi];
      }
    }
    if(ndims_yi > 1 || i == 0) {
      if(type_yi != NCL_double) { 
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,nyi,0,NULL,NULL);
      }
      else {
        tmp_yi = &((double*)yi)[index_yi];
      }
    }
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nfi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(dlinint2pts,DLININT2PTS)(&inxi,tmp_xi,&inyi,tmp_yi,tmp_fi,wrap,
                                     &inxyo,tmp_xo,tmp_yo,tmp_fo,xiw,fxiw,
                                     &inxi2,&missing_dfi.doubleval,&ier);

    if(ier) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"linint2_points: xi and yi must be monotonically increasing");
      for(j = 0; j < nxyo; j++) {
        if(type_fi == NCL_double) {
          ((double*)fo)[index_fo+j] = missing_dfi.doubleval;
        }
        else {
          ((float*)fo)[index_fo+j] = missing_rfi.floatval;
        }
      }
    }
    else {
      coerce_output_float_or_double(fo,tmp_fo,type_fi,nxyo,index_fo);
    }
    if(ndims_xi > 1) index_xi += nxi;
    if(ndims_yi > 1) index_yi += nyi;
    index_fi += nfi;
    index_fo += nxyo;
  }
/*
 * Free temp arrays.
 */
  if(type_xi != NCL_double) NclFree(tmp_xi);
  if(type_yi != NCL_double) NclFree(tmp_yi);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_fi != NCL_double) NclFree(tmp_fi);
  NclFree(tmp_fo);
  NclFree(xiw);
  NclFree(fxiw);

  if(type_fi == NCL_double) {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(fo,ndims_fi-1,dsizes_fo,&missing_dfi,NCL_double,0);
  }
  else {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(fo,ndims_fi-1,dsizes_fo,&missing_rfi,NCL_float,0);
  }
  NclFree(dsizes_fo);
  return(ret);
}

NhlErrorTypes area_hi2lores_W( void )
{
/*
 * Input variables
 */
  void *xi, *yi, *fi, *wyi, *xo, *yo;
  double *tmp_xi, *tmp_yi, *tmp_fi, *tmp_xo, *tmp_yo, *tmp_fo;
  double *tmp1_wyi, *tmp_wyi;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_wyi[1], dsizes_xo[1], dsizes_yo[1];
  int ndims_fi;
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi; 
  NclScalar missing_fi, missing_dfi, missing_rfi;
  logical *fi_cyclic_x, *fo_option;
  NclBasicDataTypes type_xi, type_yi, type_fi, type_wyi, type_xo, type_yo;
/*
 * Variables to look for attributes attached to fo_option.
 */
  NclStackEntry stack_entry;
  NclAttList  *attr_list;
  NclAtt  attr_obj;
/*
 * Output variables.
 */
  void *fo;
  ng_size_t *dsizes_fo;
  NclBasicDataTypes type_fo;
  NclScalar missing_fo;
/*
 * Other variables
 */
  int ret, ncyc = 0, ier = 0, debug = 0;
  ng_size_t i, mxi, nyi, nfi, mxo, nyo, nfo, ngrd,  size_fi, size_fo;
  int imxi, inyi, imxo, inyo, ingrd;
  double *critpc = NULL, *xilft, *xirgt, *yibot, *yitop, *xolft, *xorgt;
  double *wxi, *dxi, *dyi, *fracx, *fracy;
  double *ziwrk, *zowrk, *yiwrk, *yowrk;
  int *indx, *indy;
  NclBasicDataTypes type_critpc;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          8,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

  yi = (void*)NclGetArgValue(
          1,
          8,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

  fi = (void*)NclGetArgValue(
          2,
          8,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          DONT_CARE);

  fi_cyclic_x = (logical*)NclGetArgValue(
          3,
          8,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  wyi = (void*)NclGetArgValue(
          4,
          8,
          NULL,
          dsizes_wyi,
          NULL,
          NULL,
          &type_wyi,
          DONT_CARE);

  xo = (void*)NclGetArgValue(
          5,
          8,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

  yo = (void*)NclGetArgValue(
          6,
          8,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

  fo_option = (logical*)NclGetArgValue(
          7,
          8,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * Check for "critpc" attribute.
 */
  if(*fo_option) {
    stack_entry = _NclGetArg(7,8,DONT_CARE);
    switch(stack_entry.kind) {
    case NclStk_VAR:
      if (stack_entry.u.data_var->var.att_id != -1) {
        attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
        if (attr_obj == NULL) {
          break;
        }
      }
      else {
/*
 * att_id == -1, no attributes.
 */
        break;
      }
/* 
 * Check attributes for "critpc". If none, then just proceed as normal.
 */
      if (attr_obj->att.n_atts == 0) {
        break;
      }
      else {
/* 
 * att_n_atts > 0, retrieve optional arguments 
 */
        attr_list = attr_obj->att.att_list;
        while (attr_list != NULL) {
          if ((strcmp(attr_list->attname, "critpc")) == 0) {
            type_critpc = attr_list->attvalue->multidval.data_type;
/*
 * If "critpc" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_critpc == NCL_double) {
              critpc  = (double *)calloc(1,sizeof(double));
              *critpc = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_critpc == NCL_int || type_critpc == NCL_float) {
/*
 * Coerce to double.
 */
              critpc = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_critpc,1,0,NULL,NULL);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"area_hi2lores: The 'critpc' attribute must be of type numeric. Defaulting to 100.");
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }
  if(critpc == NULL) {
    critpc  = (double *)calloc(1,sizeof(double));
    *critpc = 100.;
  }

/*
 * Compute the total number of elements in our arrays.
 */
  mxi  = dsizes_xi[0];
  nyi  = dsizes_yi[0];
  mxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nfi  = mxi * nyi;
  nfo  = mxo * nyo;
  if(mxi < 2 || nyi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: xi and yi must have at least two elements");
    return(NhlFATAL);
  }

  if(dsizes_wyi[0] != nyi && dsizes_wyi[0] != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: wyi must be a scalar or the same length as yi");
    return(NhlFATAL);
  }
/*
 * Check dimensions of xi, yi, and fi. The last two dimensions of 
 * fi must be nyi x mxi.
 */
  if(dsizes_fi[ndims_fi-2] != nyi && dsizes_fi[ndims_fi-1] != mxi) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: The rightmost dimensions of fi must be nyi x mxi, where nyi and mxi are the lengths of yi and xi respectively");
    return(NhlFATAL);
  }

/*
 * Compute the size of the leftmost dimensions and output array.
 */
  ngrd = 1;
  for( i = 0; i < ndims_fi-2; i++ ) ngrd *= dsizes_fi[i];
  size_fi = ngrd * nfi;
  size_fo = ngrd * nfo;

/*
 * Test dimension sizes.
 */
  if((mxi > INT_MAX) || (nyi > INT_MAX) || (mxo > INT_MAX) || 
     (nyo > INT_MAX) || (ngrd > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imxi  = (int) mxi;
  inyi  = (int) nyi;
  imxo  = (int) mxo;
  inyo  = (int) nyo;
  ingrd = (int) ngrd;

/*
 * Coerce missing values for fi.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,&missing_dfi,
                 &missing_rfi);
/*
 * Allocate space for output array.
 */
  if(type_fi == NCL_double) {
    type_fo    = NCL_double;
    missing_fo = missing_dfi;
    fo         = (void*)calloc(size_fo,sizeof(double));
    if(fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_fo = fo;
  }
  else {
    type_fo    = NCL_float;
    missing_fo = missing_rfi;
    fo         = (void*)calloc(size_fo,sizeof(float));
    tmp_fo     = (double*)calloc(size_fo,sizeof(double));
    if(fo == NULL || tmp_fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  dsizes_fo = (ng_size_t*)calloc(ndims_fi,sizeof(ng_size_t));
  if(dsizes_fo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nyo;
  dsizes_fo[ndims_fi-1] = mxo;
/*
 * Coerce input arrays to double.
 */
  tmp_xi = coerce_input_double(xi,type_xi,mxi,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,nyi,0,NULL,NULL);
  tmp_fi = coerce_input_double(fi,type_fi,size_fi,0,NULL,NULL);
  tmp_xo = coerce_input_double(xo,type_xo,mxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);
/*
 * wyi can be a scalar, so copy it to array if necessary.
 */
  tmp1_wyi = coerce_input_double(wyi,type_wyi,dsizes_wyi[0],0,NULL,NULL);
  if(dsizes_wyi[0] == 1) {
    tmp_wyi = copy_scalar_to_array(tmp1_wyi,1,dsizes_wyi,nyi);
  }
  else {
    tmp_wyi = tmp1_wyi;
  }
  

/*
 * Allocate space for work arrays. There's a ton of them here.
 */
  xilft = (double*)calloc(mxi,sizeof(double));
  xirgt = (double*)calloc(mxi,sizeof(double));
  yibot = (double*)calloc(nyi,sizeof(double));
  yitop = (double*)calloc(nyi,sizeof(double));
  xolft = (double*)calloc(mxo,sizeof(double));
  xorgt = (double*)calloc(mxo,sizeof(double));
  dxi   = (double*)calloc(mxi,sizeof(double));
  dyi   = (double*)calloc(nyi,sizeof(double));
  fracx = (double*)calloc(mxi*mxo,sizeof(double));
  fracy = (double*)calloc(nyi*nyo,sizeof(double));
  ziwrk = (double*)calloc(mxi*nyi,sizeof(double));
  zowrk = (double*)calloc(mxo*nyo,sizeof(double));
  yiwrk = (double*)calloc(nyi,sizeof(double));
  yowrk = (double*)calloc(nyo,sizeof(double));
  indx  = (int*)calloc(2*mxo,sizeof(int));
  indy  = (int*)calloc(2*nyo,sizeof(int));
  wxi   = (double*)calloc(mxi,sizeof(double));

  if(xilft == NULL || xirgt == NULL || yibot == NULL || yitop == NULL || 
     xolft == NULL || xorgt == NULL || dxi   == NULL || dyi   == NULL || 
     fracx == NULL || fracy == NULL || ziwrk == NULL || zowrk == NULL || 
     yiwrk == NULL || yowrk == NULL || indx  == NULL || indy  == NULL || 
     wxi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_hi2lores: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  for(i = 0; i < mxi; i++) wxi[i] = 1.;

/*
 * Call Fortran function.
 */
  NGCALLF(arealinint2da,AREALININT2DA)(&imxi,&inyi,&ingrd,tmp_xi,tmp_yi,tmp_fi,
                                       wxi,tmp_wyi,&missing_dfi.doubleval,
                                       fi_cyclic_x,&ncyc,&imxo,&inyo,tmp_xo,
                                       tmp_yo,tmp_fo,critpc,&debug,&ier,
                                       xilft,xirgt,yibot,yitop,dyi,xolft,
                                       xorgt,yiwrk,yowrk,fracx,fracy,
                                       ziwrk,zowrk,indx,indy);

  if(ier) {
    if(ier == -2) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"area_hi2lores: xi, xo must be monotonically increasing");
    }
    else if(ier == -5) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"area_hi2lores: both dimensions of the output grid must be of lower resolution than the input high resolution grid.");
    }
    else {
/*
 * Note: we should never reach this point!  We should always know the
 * possible return values for 'ier'.
 */
      NhlPError(NhlWARNING,NhlEUNKNOWN,"area_hi2lores: unknown error, returning all missing values.");
    }
  }
  else {
    coerce_output_float_or_double(fo,tmp_fo,type_fo,size_fo,0);
  }
/*
 * Free temp arrays.
 */
  if(type_xi != NCL_double) NclFree(tmp_xi);
  if(type_yi != NCL_double) NclFree(tmp_yi);
  if(type_fi != NCL_double) NclFree(tmp_fi);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_fo != NCL_double) NclFree(tmp_fo);
  if(type_wyi != NCL_double) NclFree(tmp1_wyi);
  if(dsizes_wyi[0] == 1) {
    NclFree(tmp_wyi);
  }
  NclFree(wxi);
  NclFree(xilft);
  NclFree(xirgt);
  NclFree(yibot);
  NclFree(yitop);
  NclFree(xolft);
  NclFree(xorgt);
  NclFree(dxi);
  NclFree(dyi);
  NclFree(fracx);
  NclFree(fracy);
  NclFree(ziwrk);
  NclFree(zowrk);
  NclFree(yiwrk);
  NclFree(yowrk);
  NclFree(indx);
  NclFree(indy);
  NclFree(critpc);

  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_fo,type_fo,0);
  NclFree(dsizes_fo);
  return(ret);
}

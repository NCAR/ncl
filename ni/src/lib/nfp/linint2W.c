#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(dlinint2,DLININT2)(int *,double *,int *,double *,
                                       double *,int *,int *,double *,int *,
                                       double *,double *,double *,int *);

NhlErrorTypes linint2_W( void )
{
/*
 * Input variables
 */
  void *xi, *yi, *fi, *xo, *yo;
  double *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_fi, *tmp_fo;
  int dsizes_xi[NCL_MAX_DIMENSIONS], dsizes_yi[NCL_MAX_DIMENSIONS];
  int dsizes_xo[NCL_MAX_DIMENSIONS], dsizes_yo[NCL_MAX_DIMENSIONS];
  int ndims_fi, dsizes_fi[NCL_MAX_DIMENSIONS], has_missing_fi;
  int *dsizes_fo;
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
  int nxi, nyi, nfi, nxo, nyo, nfo, size_leftmost, size_fo;
  int i, j, index_fi, index_fo, ier;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  xi = (void*)NclGetArgValue(
          0,
          7,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

  yi = (void*)NclGetArgValue(
          1,
          7,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

  fi = (void*)NclGetArgValue(
          2,
          7,
          &ndims_fi,
          dsizes_fi,
          &missing_fi,
          &has_missing_fi,
          &type_fi,
          2);

  wrap = (logical*)NclGetArgValue(
          3,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  xo = (void*)NclGetArgValue(
          4,
          7,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

  yo = (void*)NclGetArgValue(
          5,
          7,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

  opt = (int*)NclGetArgValue(
          6,
          7,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * Compute the total number of elements in our arrays.
 */
  nxi = dsizes_xi[0];
  nyi = dsizes_yi[0];
  nxo = dsizes_xo[0];
  nyo = dsizes_yo[0];
  nfi = nxi * nyi;
  nfo = nxo * nyo;
/*
 * Check dimensions of fi.
 */
  if(dsizes_fi[ndims_fi-2] != nyi || dsizes_fi[ndims_fi-1] != nxi) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: The rightmost dimensions of fi must be nyi x nxi, where nyi and nxi are the dimensions of yi and xi respectively");
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
  dsizes_fo = (int*)calloc(ndims_fi,sizeof(int));
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
 * Coerce input arrays to double if necessary.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(nxi,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
    coerce_subset_input_double(xi,tmp_xi,0,type_xi,nxi,0,NULL,NULL);
  }
  else {
    tmp_xi = &((double*)xi)[0];
  }

  if(type_yi != NCL_double) {
    tmp_yi = (double*)calloc(nyi,sizeof(double));
    if(tmp_yi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
    coerce_subset_input_double(yi,tmp_yi,0,type_yi,nyi,0,NULL,NULL);
  }
  else {
    tmp_yi = &((double*)yi)[0];
  }

  if(type_xo != NCL_double) {
    tmp_xo = (double*)calloc(nxo,sizeof(double));
    if(tmp_xo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
    coerce_subset_input_double(xo,tmp_xo,0,type_xo,nxo,0,NULL,NULL);
  }
  else {
    tmp_xo = &((double*)xo)[0];
  }

  if(type_yo != NCL_double) {
    tmp_yo = (double*)calloc(nyo,sizeof(double));
    if(tmp_yo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"linint2: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
    coerce_subset_input_double(yo,tmp_yo,0,type_yo,nyo,0,NULL,NULL);
  }
  else {
    tmp_yo = &((double*)yo)[0];
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
  index_fi = index_fo = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_fi != NCL_double) { 
      coerce_subset_input_double(fi,tmp_fi,index_fi,type_fi,nfi,0,NULL,NULL);
    }
    else {
      tmp_fi = &((double*)fi)[index_fi];
    }

    NGCALLF(dlinint2,DLININT2)(&nxi,tmp_xi,&nyi,tmp_yi,tmp_fi,wrap,&nxo,
                               tmp_xo,&nyo,tmp_yo,tmp_fo,
                               &missing_dfi.doubleval,&ier);

    for(j = 0; j < nfo; j++) {
      if(type_fi == NCL_double) {
        ((double*)fo)[index_fo+j] = tmp_fo[j];
      }
      else {
        ((float*)fo)[index_fo+j] = (float)(tmp_fo[j]);
      }
    }
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

  if(type_fi == NCL_double) {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_dfi,NCL_double,0));
  }
  else {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(fo,ndims_fi,dsizes_fo,&missing_rfi,NCL_float,0));
  }
}

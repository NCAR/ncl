#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(skewty,SKEWTY)(double*,int*,double*);
extern void NGCALLF(skewtx,SKEWTX)(double*,double*,int*,double*);
extern double NGCALLF(tmrskewt,TMRSKEWT)(double*,double*);
extern double NGCALLF(tdaskewt,TMRSKEWT)(double*,double*);
extern double NGCALLF(satlftskewt,SATLFTSKEWT)(double*,double*);
extern void NGCALLF(ptlclskewt,PTLCLSKEWT)(double*,double*,double*,
					   double*,double*);
extern double NGCALLF(showalskewt,SHOWALSKEWT)(double*,double*,double*,int*);
extern double NGCALLF(pwskewt,SHOWALSKEWT)(double*,double*,int*);
extern double NGCALLF(capencl,CAPENCL)(double*,double*,int*,double*,int*,
				       double*,double*,int*,int*,int*);

NhlErrorTypes y_skewt_W( void )
{
/*
 * Input array variables
 */
  void *pres;
  int np;
  double *tmp_pres;
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pres;
/*
 * Output array variables
 */
  void *yskewt;
  double *tmp_yskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_pres, size_leftmost, size_pres;
/*
 * Retrieve arguments.
 */
  pres = (void*)NclGetArgValue(
          0,
          1,
          &ndims_pres,
          dsizes_pres,
          NULL,
          NULL,
          &type_pres,
          2);
/*
 * Compute the total size of the output array.
 */
  np = dsizes_pres[ndims_pres-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_pres-1; i++ ) size_leftmost *= dsizes_pres[i];
  size_pres = size_leftmost * np;

/*
 * Create a temporary array to hold subarray of pres and yskewt.
 */
  tmp_yskewt = (double*)calloc(np,sizeof(double));
  if(tmp_yskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_pres != NCL_double) {
    tmp_pres = (double*)calloc(np,sizeof(double));
    if(tmp_pres == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array
 */
  if(type_pres != NCL_double) {
    yskewt = (void*)calloc(size_pres,sizeof(float));
  }
  else {
    yskewt = (void*)calloc(size_pres,sizeof(double));
  }
  if( yskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  index_pres = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of pres (tmp_pres) to double.
 */
    if(type_pres != NCL_double) {
      coerce_subset_input_double(pres,tmp_pres,index_pres,type_pres,np,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_pres to appropriate locations in pres.
 */
      tmp_pres = &((double*)pres)[index_pres];
    }
/*
 * Call Fortran routine.
 */
    NGCALLF(skewty,SKEWTY)(tmp_pres,&np,tmp_yskewt);

    for(j = 0; j < np; j++) {
      if(type_pres != NCL_double) {
	((float*)yskewt)[index_pres+j] = (float)(tmp_yskewt[j]);
      }
      else {
	((double*)yskewt)[index_pres+j] = tmp_yskewt[j];
      }
    }
    index_pres += np;
  }
/*
 * Free memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  NclFree(tmp_yskewt);

  return(NclReturnValue(yskewt,ndims_pres,dsizes_pres,NULL,type_pres,0));
}

NhlErrorTypes x_skewt_W( void )
{
/*
 * Input array variables
 */
  void *temp, *y;
  int nty;
  double *tmp_temp, *tmp_y;
  int ndims_temp, dsizes_temp[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_temp, type_y;
/*
 * Output array variables
 */
  void *xskewt;
  double *tmp_xskewt;
  NclBasicDataTypes type_xskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_temp, size_leftmost, size_temp;
/*
 * Retrieve arguments.
 */
  temp = (void*)NclGetArgValue(
          0,
          2,
          &ndims_temp,
          dsizes_temp,
          NULL,
          NULL,
          &type_temp,
          2);
  y = (void*)NclGetArgValue(
          1,
          2,
          &ndims_y,
          dsizes_y,
          NULL,
          NULL,
          &type_y,
          2);
/*
 * temp and y must be the same size.
 */
  if(ndims_temp != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_temp; i++ ) {
    if(dsizes_temp[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the output array.
 */
  nty = dsizes_temp[ndims_temp-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_temp-1; i++ ) size_leftmost *= dsizes_temp[i];
  size_temp = size_leftmost * nty;

/*
 * Create a temporary array to hold subarray of temp, y, and xskewt.
 */
  tmp_xskewt = (double*)calloc(nty,sizeof(double));
  if(tmp_xskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_temp != NCL_double) {
    tmp_temp = (double*)calloc(nty,sizeof(double));
    if(tmp_temp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for coercing temp array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(nty,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array
 */
  if(type_temp == NCL_double || type_y == NCL_double) {
    type_xskewt = NCL_double;
    xskewt = (void*)calloc(size_temp,sizeof(double));
  }
  else {
    type_xskewt = NCL_float;
    xskewt = (void*)calloc(size_temp,sizeof(float));
  }
  if( xskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  index_temp = 0;
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of temp (tmp_temp) to double.
 */
    if(type_temp != NCL_double) {
      coerce_subset_input_double(temp,tmp_temp,index_temp,type_temp,nty,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_temp to appropriate locations in temp.
 */
      tmp_temp = &((double*)temp)[index_temp];
    }
/*
 * Coerce subsection of y (tmp_y) to double.
 */
    if(type_y != NCL_double) {
      coerce_subset_input_double(y,tmp_y,index_temp,type_y,nty,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_y to appropriate locations in y.
 */
      tmp_y = &((double*)y)[index_temp];
    }
/*
 * Call Fortran routine.
 */
    NGCALLF(skewtx,SKEWTX)(tmp_temp,tmp_y,&nty,tmp_xskewt);

    for(j = 0; j < nty; j++) {
      if(type_xskewt == NCL_double) {
	((double*)xskewt)[index_temp+j] = tmp_xskewt[j];
      }
      else {
	((float*)xskewt)[index_temp+j] = (float)(tmp_xskewt[j]);
      }
    }
    index_temp += nty;
  }
/*
 * Free memory.
 */
  if(type_temp != NCL_double) NclFree(tmp_temp);
  if(type_y    != NCL_double) NclFree(tmp_y);
  NclFree(tmp_xskewt);

  return(NclReturnValue(xskewt,ndims_temp,dsizes_temp,NULL,type_xskewt,0));
}

NhlErrorTypes tmr_skewt_W( void )
{
/*
 * Input array variables
 */
  void *w, *p;
  double *tmp_w, *tmp_p;
  int ndims_w, dsizes_w[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_w, type_p;
/*
 * Output array variables
 */
  void *tmrskewt;
  double *tmp_tmrskewt;
  NclBasicDataTypes type_tmrskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, size_w;
/*
 * Retrieve arguments.
 */
  w = (void*)NclGetArgValue(
          0,
          2,
          &ndims_w,
          dsizes_w,
          NULL,
          NULL,
          &type_w,
          2);
  p = (void*)NclGetArgValue(
          1,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * w and p must be the same size.
 */
  if(ndims_w != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_w; i++ ) {
    if(dsizes_w[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  size_w = 1;
  for( i = 0; i < ndims_w; i++ ) size_w *= dsizes_w[i];

/*
 * Create temp arrays to hold subarray of w, y, and tmrskewt.
 */
  tmp_tmrskewt = (double*)calloc(1,sizeof(double));
  if(tmp_tmrskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_w != NCL_double) {
    tmp_w = (double*)calloc(1,sizeof(double));
    if(tmp_w == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for coercing w to double precision");
      return(NhlFATAL);
    }
  }
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for coercing y to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array
 */
  if(type_w == NCL_double || type_p == NCL_double) {
    type_tmrskewt = NCL_double;
    tmrskewt = (void*)calloc(size_w,sizeof(double));
  }
  else {
    type_tmrskewt = NCL_float;
    tmrskewt = (void*)calloc(size_w,sizeof(float));
  }
  if( tmrskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_w; i++ ) {
/*
 * Coerce subsection of w (tmp_w) to double.
 */
    if(type_w != NCL_double) {
      coerce_subset_input_double(w,tmp_w,i,type_w,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_w to appropriate locations in w.
 */
      tmp_w = &((double*)w)[i];
    }
/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[i];
    }
/*
 * Call Fortran routine.
 */
    *tmp_tmrskewt = NGCALLF(tmrskewt,TMRSKEWT)(tmp_w,tmp_p);

    if(type_tmrskewt == NCL_double) {
      ((double*)tmrskewt)[i] = *tmp_tmrskewt;
    }
    else {
      ((float*)tmrskewt)[i] = (float)(*tmp_tmrskewt);
    }
  }
/*
 * Free memory.
 */
  if(type_w != NCL_double) NclFree(tmp_w);
  if(type_p != NCL_double) NclFree(tmp_p);
  NclFree(tmp_tmrskewt);

  return(NclReturnValue(tmrskewt,ndims_w,dsizes_w,NULL,type_tmrskewt,0));
}

NhlErrorTypes tda_skewt_W( void )
{
/*
 * Input array variables
 */
  void *o, *p;
  double *tmp_o, *tmp_p;
  int ndims_o, dsizes_o[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_o, type_p;
/*
 * Output array variables
 */
  void *tdaskewt;
  double *tmp_tdaskewt;
  NclBasicDataTypes type_tdaskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, size_o;
/*
 * Retrieve arguments.
 */
  o = (void*)NclGetArgValue(
          0,
          2,
          &ndims_o,
          dsizes_o,
          NULL,
          NULL,
          &type_o,
          2);
  p = (void*)NclGetArgValue(
          1,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * o and p must be the same size.
 */
  if(ndims_o != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_o; i++ ) {
    if(dsizes_o[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  size_o = 1;
  for( i = 0; i < ndims_o; i++ ) size_o *= dsizes_o[i];

/*
 * Create temp arrays to hold subarray of o, y, and tdaskewt.
 */
  tmp_tdaskewt = (double*)calloc(1,sizeof(double));
  if(tmp_tdaskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }
  if(type_o != NCL_double) {
    tmp_o = (double*)calloc(1,sizeof(double));
    if(tmp_o == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for coercing o to double precision");
      return(NhlFATAL);
    }
  }

  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for coercing y to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array
 */
  if(type_o == NCL_double || type_p == NCL_double) {
    type_tdaskewt = NCL_double;
    tdaskewt = (void*)calloc(size_o,sizeof(double));
  }
  else {
    type_tdaskewt = NCL_float;
    tdaskewt = (void*)calloc(size_o,sizeof(float));
  }
  if( tdaskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_o; i++ ) {
/*
 * Coerce subsection of o (tmp_o) to double.
 */
    if(type_o != NCL_double) {
      coerce_subset_input_double(o,tmp_o,i,type_o,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_o to appropriate locations in o.
 */
      tmp_o = &((double*)o)[i];
    }
/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[i];
    }
/*
 * Call Fortran routine.
 */
    *tmp_tdaskewt = NGCALLF(tdaskewt,TDASKEWT)(tmp_o,tmp_p);

    if(type_tdaskewt == NCL_double) {
      ((double*)tdaskewt)[i] = *tmp_tdaskewt;
    }
    else {
      ((float*)tdaskewt)[i] = (float)(*tmp_tdaskewt);
    }
  }
/*
 * Free memory.
 */
  if(type_o != NCL_double) NclFree(tmp_o);
  if(type_p != NCL_double) NclFree(tmp_p);
  NclFree(tmp_tdaskewt);

  return(NclReturnValue(tdaskewt,ndims_o,dsizes_o,NULL,type_tdaskewt,0));
}

NhlErrorTypes satlft_skewt_W( void )
{
/*
 * Input array variables
 */
  void *thw, *p;
  double *tmp_thw, *tmp_p;
  int ndims_thw, dsizes_thw[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_thw, type_p;
/*
 * Output array variables
 */
  void *satlftskewt;
  double *tmp_satlftskewt;
  NclBasicDataTypes type_satlftskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, size_thw;
/*
 * Retrieve arguments.
 */
  thw = (void*)NclGetArgValue(
          0,
          2,
          &ndims_thw,
          dsizes_thw,
          NULL,
          NULL,
          &type_thw,
          2);
  p = (void*)NclGetArgValue(
          1,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * thw and p must be the same size.
 */
  if(ndims_thw != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_thw; i++ ) {
    if(dsizes_thw[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  size_thw = 1;
  for( i = 0; i < ndims_thw; i++ ) size_thw *= dsizes_thw[i];

/*
 * Create temp arrays to hold subarray of o, y, and satlftskewt.
 */
  tmp_satlftskewt = (double*)calloc(1,sizeof(double));
  if(tmp_satlftskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }
  if(type_thw != NCL_double) {
    tmp_thw = (double*)calloc(1,sizeof(double));
    if(tmp_thw == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for coercing thw to double precision");
      return(NhlFATAL);
    }
  }

  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for output array
 */
  if(type_thw == NCL_double || type_p == NCL_double) {
    type_satlftskewt = NCL_double;
    satlftskewt = (void*)calloc(size_thw,sizeof(double));
  }
  else {
    type_satlftskewt = NCL_float;
    satlftskewt = (void*)calloc(size_thw,sizeof(float));
  }
  if( satlftskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_thw; i++ ) {
/*
 * Coerce subsection of thw (tmp_thw) to double.
 */
    if(type_thw != NCL_double) {
      coerce_subset_input_double(thw,tmp_thw,i,type_thw,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_thw to appropriate locations in thw.
 */
      tmp_thw = &((double*)thw)[i];
    }
/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[i];
    }
/*
 * Call Fortran routine.
 */
    *tmp_satlftskewt = NGCALLF(satlftskewt,SATLFTSKEWT)(tmp_thw,tmp_p);

    if(type_satlftskewt == NCL_double) {
      ((double*)satlftskewt)[i] = *tmp_satlftskewt;
    }
    else {
      ((float*)satlftskewt)[i] = (float)(*tmp_satlftskewt);
    }
  }
/*
 * Free memory.
 */
  if(type_thw != NCL_double) NclFree(tmp_thw);
  if(type_p   != NCL_double) NclFree(tmp_p);
  NclFree(tmp_satlftskewt);

  return(NclReturnValue(satlftskewt,ndims_thw,dsizes_thw,NULL,type_satlftskewt,0));
}

NhlErrorTypes ptlcl_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *td;
  double *tmp_t, *tmp_p, *tmp_td;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_td, dsizes_td[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_p, type_td;
/*
 * Output array variables
 */
  void *pc, *tc;
  double *tmp_pc, *tmp_tc;
  int ndims_pc, dsizes_pc[NCL_MAX_DIMENSIONS];
  int ndims_tc, dsizes_tc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ptlclskewt, type_pc, type_tc;
/*
 * Declare various variables for random purposes.
 */
  int i, j, size_t;
/*
 * Retrieve arguments.
 */
  p = (void*)NclGetArgValue(
          0,
          5,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
  t = (void*)NclGetArgValue(
          1,
          5,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);
  td = (void*)NclGetArgValue(
          2,
          5,
          &ndims_td,
          dsizes_td,
          NULL,
          NULL,
          &type_td,
          2);
  pc = (void*)NclGetArgValue(
          3,
          5,
          &ndims_pc,
          dsizes_pc,
          NULL,
          NULL,
          &type_pc,
          2);
  tc = (void*)NclGetArgValue(
          4,
          5,
          &ndims_tc,
          dsizes_tc,
          NULL,
          NULL,
          &type_tc,
          2);
/*
 * arrays must be the same size.
 */
  if(ndims_t != ndims_p || ndims_t != ndims_td || ndims_t != ndims_pc ||
     ndims_t != ndims_tc) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_t; i++ ) {
    if(dsizes_t[i] !=  dsizes_p[i] || dsizes_t[i] != dsizes_td[i] ||
       dsizes_t[i] != dsizes_pc[i] || dsizes_t[i] != dsizes_tc[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Output variables must be float or double.
 */
  if((type_pc != NCL_float && type_pc != NCL_double) ||
     (type_tc != NCL_float && type_tc != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: pc and tc must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total size of the input arrays.
 */
  size_t = 1;
  for( i = 0; i < ndims_t; i++ ) size_t *= dsizes_t[i];

/*
 * Create temp arrays to hold subarray of p, t, td, pc, and tc.
 */
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(1,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing t to double precision");
      return(NhlFATAL);
    }
  }

  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(1,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
  }

  if(type_td != NCL_double) {
    tmp_td = (double*)calloc(1,sizeof(double));
    if(tmp_td == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing td to double precision");
      return(NhlFATAL);
    }
  }

  if(type_pc == NCL_float) {
    tmp_pc = (double*)calloc(1,sizeof(double));
    if(tmp_pc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing pc to double precision");
      return(NhlFATAL);
    }
  }

  if(type_tc == NCL_float) {
    tmp_tc = (double*)calloc(1,sizeof(double));
    if(tmp_tc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing tc to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_t; i++ ) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,i,type_t,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate locations in t.
 */
      tmp_t = &((double*)t)[i];
    }

/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,i,type_p,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[i];
    }
/*
 * Coerce subsection of td (tmp_td) to double.
 */
    if(type_td != NCL_double) {
      coerce_subset_input_double(td,tmp_td,i,type_td,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_td to appropriate locations in td.
 */
      tmp_td = &((double*)td)[i];
    }
/*
 * Call Fortran routine.
 */
    if(type_pc == NCL_double) tmp_pc = &((double*)pc)[i];
    if(type_tc == NCL_double) tmp_tc = &((double*)tc)[i];

    NGCALLF(ptlclskewt,PTLCLSKEWT)(tmp_p,tmp_t,tmp_td,tmp_pc,tmp_tc);

    if(type_pc != NCL_double) ((float*)pc)[i] = (float)*tmp_pc;
    if(type_tc != NCL_double) ((float*)tc)[i] = (float)*tmp_tc;
  }
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  if(type_pc != NCL_double) NclFree(tmp_pc);
  if(type_tc != NCL_double) NclFree(tmp_tc);

  return(NhlNOERROR);
}

NhlErrorTypes showal_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *td;
  double *tmp_t, *tmp_p, *tmp_td;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_td, dsizes_td[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_p, type_td;
/*
 * Output array variables
 */
  void *showalskewt;
  double *tmp_showalskewt;
  NclBasicDataTypes type_showalskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, nlvls, index_t, size_leftmost;
/*
 * Retrieve arguments.
 */
  p = (void*)NclGetArgValue(
          0,
          3,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
  t = (void*)NclGetArgValue(
          1,
          3,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);
  td = (void*)NclGetArgValue(
          2,
          3,
          &ndims_td,
          dsizes_td,
          NULL,
          NULL,
          &type_td,
          2);
/*
 * input arrays must be the same size.
 */
  if(ndims_t != ndims_p || ndims_t != ndims_td) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_t; i++ ) {
    if(dsizes_t[i] !=  dsizes_p[i] || dsizes_t[i] != dsizes_td[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  nlvls = dsizes_t[ndims_t-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_t-1; i++ ) size_leftmost *= dsizes_t[i];

/*
 * Create temp arrays to hold subarray of p, t, td.
 */
  tmp_showalskewt = (double*)calloc(1,sizeof(double));
  if(tmp_showalskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(nlvls,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for coercing t to double precision");
      return(NhlFATAL);
    }
  }

  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(nlvls,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
  }

  if(type_td != NCL_double) {
    tmp_td = (double*)calloc(nlvls,sizeof(double));
    if(tmp_td == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for coercing td to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array
 */
  if(type_t == NCL_double || type_p == NCL_double || type_td == NCL_double) {
    type_showalskewt = NCL_double;
    showalskewt = (void*)calloc(size_leftmost,sizeof(double));
  }
  else {
    type_showalskewt = NCL_float;
    showalskewt = (void*)calloc(size_leftmost,sizeof(float));
  }
  if( showalskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_t,type_t,nlvls,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate locations in t.
 */
      tmp_t = &((double*)t)[index_t];
    }

/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_t,type_p,nlvls,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[index_t];
    }
/*
 * Coerce subsection of td (tmp_td) to double.
 */
    if(type_td != NCL_double) {
      coerce_subset_input_double(td,tmp_td,index_t,type_td,nlvls,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_td to appropriate locations in td.n
 */
      tmp_td = &((double*)td)[index_t];
    }
/*
 * Call Fortran routine.
 */
    *tmp_showalskewt = NGCALLF(showalskewt,SHOWALSKEWT)(tmp_p,tmp_t,tmp_td,
							&nlvls);
    if(type_showalskewt == NCL_double) {
      ((double*)showalskewt)[i] = *tmp_showalskewt;
    }
    else {
      ((float*)showalskewt)[i] = (float)(*tmp_showalskewt);
    }
    index_t += nlvls;
  }
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  NclFree(tmp_showalskewt);

  return(NclReturnValue(showalskewt,ndims_t,dsizes_t,NULL,type_showalskewt,0));
}

NhlErrorTypes pw_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *td;
  double *tmp_p, *tmp_td;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_td, dsizes_td[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_td;
/*
 * Output array variables
 */
  void *pwskewt;
  double *tmp_pwskewt;
  NclBasicDataTypes type_pwskewt;
/*
 * Declare various variables for random purposes.
 */
  int i, j, n, index_td, size_td, size_leftmost;
/*
 * Retrieve arguments.
 */
  td = (void*)NclGetArgValue(
          0,
          2,
          &ndims_td,
          dsizes_td,
          NULL,
          NULL,
          &type_td,
          2);
  p = (void*)NclGetArgValue(
          1,
          2,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);
/*
 * input arrays must be the same size.
 */
  if(ndims_td != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: The input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_td; i++ ) {
    if(dsizes_td[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  n = dsizes_td[ndims_td-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_td-1; i++ ) size_leftmost *= dsizes_td[i];

/*
 * Create temp arrays to hold subarray of p, t, td.
 */
  tmp_pwskewt = (double*)calloc(1,sizeof(double));
  if(tmp_pwskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(n,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
  }

  if(type_td != NCL_double) {
    tmp_td = (double*)calloc(n,sizeof(double));
    if(tmp_td == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for coercing td to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array
 */
  if(type_td == NCL_double || type_p == NCL_double) {
    type_pwskewt = NCL_double;
    pwskewt = (void*)calloc(size_leftmost,sizeof(double));
  }
  else {
    type_pwskewt = NCL_float;
    pwskewt = (void*)calloc(size_leftmost,sizeof(float));
  }
  if( pwskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of p (tmp_p) to double.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_td,type_p,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate locations in p.
 */
      tmp_p = &((double*)p)[index_td];
    }
/*
 * Coerce subsection of td (tmp_td) to double.
 */
    if(type_td != NCL_double) {
      coerce_subset_input_double(td,tmp_td,index_td,type_td,n,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_td to appropriate locations in td.n
 */
      tmp_td = &((double*)td)[index_td];
    }
/*
 * Call Fortran routine.
 */
    *tmp_pwskewt = NGCALLF(pwskewt,PWSKEWT)(tmp_td,tmp_p,&n);
    if(type_pwskewt == NCL_double) {
      ((double*)pwskewt)[i] = *tmp_pwskewt;
    }
    else {
      ((float*)pwskewt)[i] = (float)(*tmp_pwskewt);
    }
    index_td += n;
  }
/*
 * Free memory.
 */
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  NclFree(tmp_pwskewt);

  return(NclReturnValue(pwskewt,ndims_td,dsizes_td,NULL,type_pwskewt,0));
}

NhlErrorTypes cape_thermo_W( void )
{
/*
 * Input array variables
 */
  void *penv, *tenv, *lclmb;
  int *iprint;
  double *tmp_tenv, *tmp_penv, *tmp_lclmb;
  int ndims_tenv, dsizes_tenv[NCL_MAX_DIMENSIONS];
  int ndims_penv, dsizes_penv[NCL_MAX_DIMENSIONS];
  int ndims_lclmb, dsizes_lclmb[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_tenv, type_penv, type_lclmb;
/*
 * Output array variables
 */
  void *cape;
  double *tmp_cape;
  NclBasicDataTypes type_cape;
/*
 * Declare various variables for random purposes.
 */
  double *tparcel, *tmsg;
  int *jlcl, *jlfc, *jcross;
  int i, j, nlvls, index_tenv, size_leftmost;
/*
 * Retrieve arguments.
 */
  penv = (void*)NclGetArgValue(
          0,
          4,
          &ndims_penv,
          dsizes_penv,
          NULL,
          NULL,
          &type_penv,
          2);
  tenv = (void*)NclGetArgValue(
          1,
          4,
          &ndims_tenv,
          dsizes_tenv,
          NULL,
          NULL,
          &type_tenv,
          2);
  lclmb = (void*)NclGetArgValue(
          2,
          4,
          &ndims_lclmb,
          dsizes_lclmb,
          NULL,
          NULL,
          &type_lclmb,
          2);
  iprint = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
/*
 * tenv and penv must be the same size.
 */
  if(ndims_tenv != ndims_penv) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: tenv and penv must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_tenv; i++ ) {
    if(dsizes_tenv[i] !=  dsizes_penv[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: tenv and penv must have the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the input arrays.
 */
  nlvls = dsizes_tenv[ndims_tenv-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_tenv-1; i++ ) size_leftmost *= dsizes_tenv[i];

/*
 * Create temp arrays to hold subarray of penv, tenv, lclmb.
 */
  tmp_cape = (double*)calloc(size_leftmost,sizeof(double));
  if(tmp_cape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output");
    return(NhlFATAL);
  }
  if(type_tenv != NCL_double) {
    tmp_tenv = (double*)calloc(nlvls,sizeof(double));
    if(tmp_tenv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for coercing t to double precision");
      return(NhlFATAL);
    }
  }

  if(type_penv != NCL_double) {
    tmp_penv = (double*)calloc(nlvls,sizeof(double));
    if(tmp_penv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for coercing p to double precision");
      return(NhlFATAL);
    }
  }

  if(type_lclmb != NCL_double) {
    tmp_lclmb = (double*)calloc(1,sizeof(double));
    if(tmp_lclmb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for coercing lclmb to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array
 */
  if(type_tenv == NCL_double || type_penv == NCL_double) {
    type_cape = NCL_double;
    cape = (void*)calloc(size_leftmost,sizeof(double));
  }
  else {
    type_cape = NCL_float;
    cape = (void*)calloc(size_leftmost,sizeof(float));
  }
  if( cape == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output");
    return(NhlFATAL);
  }
/*
 * Allocate space for dummy variables.
 */
  tparcel = (double*)calloc(nlvls,sizeof(double));
  tmsg    = (double*)calloc(1,sizeof(double));
  jlcl    = (int*)calloc(1,sizeof(int));
  jlfc    = (int*)calloc(1,sizeof(int));
  jcross  = (int*)calloc(1,sizeof(int));
  if(tparcel == NULL || tmsg == NULL || jlcl == NULL || jlfc == NULL || 
      jcross == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }


/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce subsection of tenv (tmp_tenv) to double.
 */
    if(type_tenv != NCL_double) {
      coerce_subset_input_double(tenv,tmp_tenv,index_tenv,type_tenv,nlvls,
				 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_tenv to appropriate locations in tenv.
 */
      tmp_tenv = &((double*)tenv)[index_tenv];
    }

/*
 * Coerce subsection of penv (tmp_penv) to double.
 */
    if(type_penv != NCL_double) {
      coerce_subset_input_double(penv,tmp_penv,index_tenv,type_penv,nlvls,
				 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_penv to appropriate locations in penv.
 */
      tmp_penv = &((double*)penv)[index_tenv];
    }

/*
 * Coerce subsection of lclmb (tmp_lclmb) to double.
 */
    if(type_lclmb != NCL_double) {
      coerce_subset_input_double(lclmb,tmp_lclmb,i,type_lclmb,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_lclmb to appropriate locations in lclmb.n
 */
      tmp_lclmb = &((double*)lclmb)[i];
    }
/*
 * Call Fortran routine.
 */
    *tmp_cape = NGCALLF(capencl,CAPENCL)(tmp_penv,tmp_tenv,&nlvls,tmp_lclmb,
					 iprint,tparcel,tmsg,jlcl,jlfc,
					 jcross);
    if(type_cape == NCL_double) {
      ((double*)cape)[i] = *tmp_cape;
    }
    else {
      ((float*)cape)[i] = (float)(*tmp_cape);
    }
    index_tenv += nlvls;
  }
/*
 * Free memory.
 */
  if(type_tenv  != NCL_double) NclFree(tmp_tenv);
  if(type_penv  != NCL_double) NclFree(tmp_penv);
  if(type_lclmb != NCL_double) NclFree(tmp_lclmb);
  NclFree(tmp_cape);

  return(NclReturnValue(cape,ndims_lclmb,dsizes_lclmb,NULL,type_cape,0));
}


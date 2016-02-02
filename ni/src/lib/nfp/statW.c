#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dstat2,DSTAT2)(double*, int*, double*, double*, double*, 
                                   double*, int*, int*); 

extern void NGCALLF(dstat4,DSTAT4)(double*, int*, double*, double*, double*, 
                                   double*, double*, double*, int*, int*); 

extern void NGCALLF(dstat2t,DSTAT2T)(double *,int *, double *,double *,
                                     double *,double *,int *,double *,
                                     double *, int*);

extern void NGCALLF(drmvmean,DRMVMEAN)(double*, int*, double*, int*);

extern void NGCALLF(drmvmed,DRMVMED)(double*, double*, int*, double*, int*);

extern void NGCALLF(dmedmrng,DMEDMRNG)(double*, double*, int*, double*, 
                                       double*, double*, double*, int*, 
                                       int*); 

extern void NGCALLF(drmsd,DRMSD)(double *, double *, int *, double *, 
                                 double *, double *, int *, int *);

extern void NGCALLF(dxstnd,DXSTND)(double*, int*, double*, int*, int*);

extern void NGCALLF(desauto,DESAUTO)(double*, int*, double*, double*, 
                                     double*, int*, double*, double*, int*);

extern void NGCALLF(descros,DESCROS)(double*, double*, int*, double*, 
                                     double*, double*, double*, double*,
                                     double*, int*, double*, double*, int*);

extern void NGCALLF(dacumrun,DACUMRUN)(double *,int *,double *,int *,
                                       double *, int *, int *);

NhlErrorTypes stat2_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmean, *xvar;
  double *tmp_xmean = NULL;
  double *tmp_xvar = NULL;
  int *nptused;
  int ndims_xmean;
  ng_size_t dsizes_xmean[NCL_MAX_DIMENSIONS];
  int ndims_xvar;
  ng_size_t dsizes_xvar[NCL_MAX_DIMENSIONS];
  int ndims_nptused;
  ng_size_t dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out;
  ng_size_t *dsizes_out;
  NclBasicDataTypes type_xmean, type_xvar;
/*
 * various
 */
  ng_size_t i, index_x, size_leftmost;
  int ier = 0, ier_count;
  ng_size_t npts;
  int inpts;
  double xsd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
/*
 * Coerce missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Compute the total number of elements in our x array.
 */
  npts = dsizes_x[ndims_x-1];
  size_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) size_leftmost *= dsizes_x[i];
  
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (ng_size_t*)NclMalloc(ndims_out*sizeof(ng_size_t));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmean = (void*)NclGetArgValue(
           1,
           4,
           &ndims_xmean, 
           dsizes_xmean,
           NULL,
           NULL,
           &type_xmean,
           1);
  xvar = (void*)NclGetArgValue(
           2,
           4,
           &ndims_xvar, 
           dsizes_xvar,
           NULL,
           NULL,
           &type_xvar,
           1);
  nptused = (int*)NclGetArgValue(
           3,
           4,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

/*
 * The number of dimensions in all arrays must be the same.
 */
  if( ndims_xmean != ndims_out || ndims_nptused != ndims_out || 
      ndims_xvar != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The number of dimensions of xmean, xvar, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * The output types must be float or double.
 */
  if( (type_xmean != NCL_float && type_xmean != NCL_double ) ||
      (type_xvar  != NCL_float && type_xvar  != NCL_double )) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The types of the output arrays must all be float or double");
    return(NhlFATAL);
  }
/*
 * Dimension sizes of xmean, xvar, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
      if( dsizes_xmean[i]   != dsizes_out[i] || 
          dsizes_xvar[i]    != dsizes_out[i] || 
          dsizes_nptused[i] != dsizes_out[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: The dimensions of xmean, xvar, and nptused must be the same as the left-most dimensions of x");
        return(NhlFATAL);
      }
  }
/*
 * Create double precision arrays if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_xmean == NCL_float) {
    tmp_xmean = (double*)calloc(1,sizeof(double));
    if(tmp_xmean == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xvar == NCL_float) {
    tmp_xvar = (double*)calloc(1,sizeof(double));
    if(tmp_xvar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat2: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'stat2' with the full argument list.
 */
  index_x = ier_count = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }

    if(type_xmean == NCL_double) tmp_xmean = &((double*)xmean)[i];
    if(type_xvar  == NCL_double) tmp_xvar  = &((double*)xvar)[i];

    NGCALLF(dstat2,DSTAT2)(tmp_x,&inpts,&missing_dx.doubleval,tmp_xmean,
                           tmp_xvar,&xsd,&nptused[i],&ier);
    if (ier == 2) {
      *tmp_xmean = *tmp_xvar = missing_dx.doubleval;
      ier_count++;
    }
    if(type_xmean == NCL_float) {
      coerce_output_float_only(xmean,tmp_xmean,1,i);
    }
    if(type_xvar  == NCL_float) {
      coerce_output_float_only(xvar,tmp_xvar,1,i);
    }
    index_x += npts;
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"stat2: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }
/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_xmean != NCL_double) NclFree(tmp_xmean);
  if(type_xvar  != NCL_double) NclFree(tmp_xvar);
  NclFree(dsizes_out);
  
  return(NhlNOERROR);
}

NhlErrorTypes stat_trim_W( void )
{
/*
 * Input array variables
 */
  void *x, *ptrim;
  double *tmp_x = NULL;
  double *tmp_ptrim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x, type_ptrim;
/*
 * Output array variables
 */
  void *xmeant, *xsdt;
  double *tmp_xmeant = NULL;
  double *tmp_xsdt = NULL;
  int *nptused;
  int ndims_xmeant;
  ng_size_t dsizes_xmeant[NCL_MAX_DIMENSIONS];
  int ndims_xsdt;
  ng_size_t dsizes_xsdt[NCL_MAX_DIMENSIONS];
  int ndims_nptused;
  ng_size_t dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out;
  ng_size_t *dsizes_out;
  NclBasicDataTypes type_xmeant, type_xsdt;
/*
 * various
 */
  ng_size_t i, index_x, size_leftmost;
  int ier = 0, ier_count;
  ng_size_t npts;
  int inpts;
  double xvart, *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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

  ptrim = (void*)NclGetArgValue(
           1,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_ptrim,
           DONT_CARE);

/*
 * Coerce ptrim to double if it isn't already.
 */
  tmp_ptrim = coerce_input_double(ptrim,type_ptrim,1,0,NULL,NULL);

  if( *tmp_ptrim < 0. || *tmp_ptrim >= 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: ptrim must be >= 0.0 and < 1.0");
    return(NhlFATAL);
  }

/*
 * Coerce missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Get size of leftmost dimensions.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) size_leftmost *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
  dsizes_out = (ng_size_t*)NclMalloc(ndims_out*sizeof(ng_size_t));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmeant = (void*)NclGetArgValue(
           2,
           5,
           &ndims_xmeant, 
           dsizes_xmeant,
           NULL,
           NULL,
           &type_xmeant,
           1);

  xsdt = (void*)NclGetArgValue(
           3,
           5,
           &ndims_xsdt, 
           dsizes_xsdt,
           NULL,
           NULL,
           &type_xsdt,
           1);

  nptused = (int*)NclGetArgValue(
           4,
           5,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

/*
 * Check # of dimensions. 
 */
  if( ndims_xmeant != ndims_out || ndims_nptused != ndims_out || 
      ndims_xsdt != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: The number of dimensions of xmeant, xsdt, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes of xmeant, xsdt, and nptused.
 */
  for(i = 0; i < ndims_out; i++ ) {
    if( dsizes_xmeant[i]  != dsizes_out[i] || 
        dsizes_xsdt[i]    != dsizes_out[i] || 
        dsizes_nptused[i] != dsizes_out[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: The dimensions of xmeant, xsdt, and nptused must be the same as the left-most dimensions of x");
      return(NhlFATAL);
    }
  }

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate space for work array" );
    return(NhlFATAL);
  }

/*
 * Create double precision arrays if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_xmeant == NCL_float) {
    tmp_xmeant = (double*)calloc(1,sizeof(double));
    if(tmp_xmeant == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xsdt == NCL_float) {
    tmp_xsdt = (double*)calloc(1,sizeof(double));
    if(tmp_xsdt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'stat_trim' with the full argument list.
 */
  index_x = ier_count = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }

    if(type_xmeant == NCL_double) tmp_xmeant = &((double*)xmeant)[i];
    if(type_xsdt   == NCL_double) tmp_xsdt   = &((double*)xsdt)[i];

    NGCALLF(dstat2t,DSTAT2T)(tmp_x,&inpts,&missing_dx.doubleval,tmp_xmeant,
                             &xvart,tmp_xsdt,&nptused[i],work,tmp_ptrim,&ier);

    if (ier == 2) {
      *tmp_xmeant = *tmp_xsdt = missing_dx.doubleval;
      ier_count++;
    }
    if (ier == 4) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_trim: not enough trimmed values");
      return(NhlFATAL);
    }
    if(type_xmeant == NCL_float) {
      coerce_output_float_only(xmeant,tmp_xmeant,1,i);
    }
    if(type_xsdt  == NCL_float) {
      coerce_output_float_only(xsdt,tmp_xsdt,1,i);
    }

    index_x += npts;
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"stat_trim: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }
/*
 * Free unneeded memory.
 */
  if(type_x      != NCL_double) NclFree(tmp_x);
  if(type_ptrim  != NCL_double) NclFree(tmp_ptrim);
  if(type_xmeant != NCL_double) NclFree(tmp_xmeant);
  if(type_xsdt   != NCL_double) NclFree(tmp_xsdt);
  NclFree(work);
  NclFree(dsizes_out);

  return(NhlNOERROR);
}


NhlErrorTypes stat4_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmean, *xvar, *xskew, *xkurt;
  double *tmp_xmean = NULL;
  double *tmp_xvar = NULL;
  double *tmp_xskew = NULL;
  double *tmp_xkurt = NULL;
  int *nptused;
  int ndims_xmean;
  ng_size_t dsizes_xmean[NCL_MAX_DIMENSIONS];
  int ndims_xskew;
  ng_size_t dsizes_xskew[NCL_MAX_DIMENSIONS];
  int ndims_xkurt;
  ng_size_t dsizes_xkurt[NCL_MAX_DIMENSIONS];
  int ndims_xvar;
  ng_size_t dsizes_xvar[NCL_MAX_DIMENSIONS];
  int ndims_nptused;
  ng_size_t dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out;
  ng_size_t *dsizes_out;
  NclBasicDataTypes type_xmean, type_xvar, type_xskew, type_xkurt;
/*
 * various
 */
  ng_size_t i, index_x, size_leftmost;
  ng_size_t npts;
  int inpts;
  int ier = 0, ier_count;
  double xsd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           6,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Coerce missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Compute the total number of elements in our x array.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) size_leftmost *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out  = max(ndims_x-1,1);
  dsizes_out = (ng_size_t*)NclMalloc(ndims_out*sizeof(ng_size_t));
  dsizes_out[0] = 1;
  for(i = 0; i < ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i];
/* 
 * Get output variables.
 */
  xmean = (void*)NclGetArgValue(
           1,
           6,
           &ndims_xmean, 
           dsizes_xmean,
           NULL,
           NULL,
           &type_xmean,
           1);
  xvar = (void*)NclGetArgValue(
           2,
           6,
           &ndims_xvar, 
           dsizes_xvar,
           NULL,
           NULL,
           &type_xvar,
           1);
  xskew = (void*)NclGetArgValue(
           3,
           6,
           &ndims_xskew, 
           dsizes_xskew,
           NULL,
           NULL,
           &type_xskew,
           1);
  xkurt = (void*)NclGetArgValue(
           4,
           6,
           &ndims_xkurt, 
           dsizes_xkurt,
           NULL,
           NULL,
           &type_xkurt,
           1);
  nptused = (int*)NclGetArgValue(
           5,
           6,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The number of dimensions in all arrays must be the same.
 */
  if( ndims_xmean != ndims_out || ndims_nptused != ndims_out || 
      ndims_xskew != ndims_out || ndims_xkurt != ndims_out || 
      ndims_xvar != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: The number of dimensions of xmean, xvar, xskew, xkurt, and nptused must be one less than the number of dimensions of x, or they must all be scalar if x is a one-dimensional array)");
    return(NhlFATAL);
  }
/*
 * The output types must be float or double.
 */
  if( (type_xmean != NCL_float && type_xmean != NCL_double ) ||
      (type_xvar  != NCL_float && type_xvar  != NCL_double ) ||
      (type_xskew != NCL_float && type_xskew != NCL_double ) ||
      (type_xkurt != NCL_float && type_xkurt != NCL_double )) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: The types of the output arrays must all be float or double");
    return(NhlFATAL);
  }
/*
 * Dimension sizes of xmean, xvar, xskew, xkurt, and nptused must be the same.
 */
  for(i = 0; i < ndims_out; i++ ) {
    if( dsizes_xmean[i]   != dsizes_out[i] || 
        dsizes_xvar[i]    != dsizes_out[i] || 
        dsizes_xskew[i]   != dsizes_out[i] ||
        dsizes_xkurt[i]   != dsizes_out[i] || 
        dsizes_nptused[i] != dsizes_out[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: xmean, xskew, xkurt, xvar, and nptused must be the same as the leftmost dimensions of x");
      return(NhlFATAL);
    }
  }
/*
 * Create double precision arrays if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_xmean == NCL_float) {
    tmp_xmean = (double*)calloc(1,sizeof(double));
    if(tmp_xmean == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xvar == NCL_float) {
    tmp_xvar = (double*)calloc(1,sizeof(double));
    if(tmp_xvar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xskew == NCL_float) {
    tmp_xskew = (double*)calloc(1,sizeof(double));
    if(tmp_xskew == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xkurt == NCL_float) {
    tmp_xkurt = (double*)calloc(1,sizeof(double));
    if(tmp_xkurt == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat4: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'stat4' with the full argument list.
 */
  index_x = ier_count = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }

    if(type_xmean == NCL_double) tmp_xmean = &((double*)xmean)[i];
    if(type_xvar  == NCL_double) tmp_xvar  = &((double*)xvar)[i];
    if(type_xskew == NCL_double) tmp_xskew = &((double*)xskew)[i];
    if(type_xkurt == NCL_double) tmp_xkurt = &((double*)xkurt)[i];
    if(type_xmean == NCL_double) tmp_xmean = &((double*)xmean)[i];

    NGCALLF(dstat4,DSTAT4)(tmp_x,&inpts,&missing_dx.doubleval,tmp_xmean,
                           tmp_xvar,&xsd,tmp_xskew,tmp_xkurt,&nptused[i],&ier);

    if (ier == 2) {
/*
 * Input was all missing for this subset, so set output to missing
 * as well.
 */
      *tmp_xmean = missing_dx.doubleval;
      *tmp_xvar  = missing_dx.doubleval;
      *tmp_xskew = missing_dx.doubleval;
      *tmp_xkurt = missing_dx.doubleval;
      ier_count++;
    }
    if(type_xmean == NCL_float) {
      coerce_output_float_only(xmean,tmp_xmean,1,i);
    }
    if(type_xvar  == NCL_float) {
      coerce_output_float_only(xvar,tmp_xvar,1,i);
    }
    if(type_xskew == NCL_float) {
      coerce_output_float_only(xskew,tmp_xskew,1,i);
    }
    if(type_xkurt == NCL_float) {
      coerce_output_float_only(xkurt,tmp_xkurt,1,i);
    }

    index_x += npts;
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"stat4: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }

/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_xmean != NCL_double) NclFree(tmp_xmean);
  if(type_xvar  != NCL_double) NclFree(tmp_xvar);
  if(type_xskew != NCL_double) NclFree(tmp_xskew);
  if(type_xkurt != NCL_double) NclFree(tmp_xkurt);
  NclFree(dsizes_out);

  return(NhlNOERROR);
}

NhlErrorTypes stat_medrng_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmedian, *xmrange, *xrange;
  double *tmp_xmedian = NULL;
  double *tmp_xmrange = NULL;
  double *tmp_xrange = NULL;
  int *nptused;
  int ndims_xmedian;
  ng_size_t dsizes_xmedian[NCL_MAX_DIMENSIONS];
  int ndims_xrange;
  ng_size_t dsizes_xrange[NCL_MAX_DIMENSIONS];
  int ndims_xmrange;
  ng_size_t dsizes_xmrange[NCL_MAX_DIMENSIONS];
  int ndims_nptused;
  ng_size_t dsizes_nptused[NCL_MAX_DIMENSIONS];
  int ndims_out;
  NclBasicDataTypes type_xmedian, type_xmrange, type_xrange;
/*
 * various
 */
  ng_size_t i, index_x, size_leftmost;
  int ier = 0, ier_count;
  ng_size_t npts;
  int inpts;
  double *work;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (double*)NclGetArgValue(
           0,
           5,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Coerce missing value.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Compute the total number of elements in our x array.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) size_leftmost *= dsizes_x[i];
/*
 * Calculate what the size is supposed to be of our output arrays.
 */
  ndims_out = max(ndims_x-1,1);
/* 
 * Get output variables.
 */
  xmedian = (void*)NclGetArgValue(
           1,
           5,
           &ndims_xmedian, 
           dsizes_xmedian,
           NULL,
           NULL,
           &type_xmedian,
           1);

  xmrange = (void*)NclGetArgValue(
           2,
           5,
           &ndims_xmrange, 
           dsizes_xmrange,
           NULL,
           NULL,
           &type_xmrange,
           1);

  xrange = (void*)NclGetArgValue(
           3,
           5,
           &ndims_xrange, 
           dsizes_xrange,
           NULL,
           NULL,
           &type_xrange,
           1);

  nptused = (int*)NclGetArgValue(
           4,
           5,
           &ndims_nptused, 
           dsizes_nptused,
           NULL,
           NULL,
           NULL,
           1);

/*
 * Check the number of dimensions.
 */
  if( ndims_xmedian != ndims_out || ndims_nptused != ndims_out || 
      ndims_xrange != ndims_out || ndims_xmrange != ndims_out ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The number of dimensions of xmedian, xmrange, xrange, and nptused must be one less than the number of dimensions of x (or they must all be scalar if x is just a 1-d array)");
    return(NhlFATAL);
  }
/*
 * Check the dimension sizes of xmedian, xrange, and nptused.
 */
  for(i = 0; i < ndims_out; i++ ) {
    if( dsizes_xmedian[i] != dsizes_x[i] || 
        dsizes_xmrange[i] != dsizes_x[i] ||
        dsizes_xrange[i]  != dsizes_x[i] || 
        dsizes_nptused[i] != dsizes_x[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The dimensions of xmedian, xrange, xmrange, and nptused must be the same as the left-most dimensions of x");
      return(NhlFATAL);
    }
  }
/*
 * The output types must be float or double.
 */
  if( (type_xmedian != NCL_float && type_xmedian != NCL_double) ||
      (type_xmrange != NCL_float && type_xmrange != NCL_double) ||
      (type_xrange  != NCL_float && type_xrange  != NCL_double) ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: The types of the output arrays must all be float or double");
    return(NhlFATAL);
  }
/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Create double precision arrays if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_xmedian == NCL_float) {
    tmp_xmedian = (double*)calloc(1,sizeof(double));
    if(tmp_xmedian == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xmrange == NCL_float) {
    tmp_xmrange = (double*)calloc(1,sizeof(double));
    if(tmp_xmrange == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
  if(type_xrange == NCL_float) {
    tmp_xrange = (double*)calloc(1,sizeof(double));
    if(tmp_xrange == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"stat_medrng: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'stat_medrng' with the full argument list.
 */
  index_x = ier_count = 0;
  for(i = 0; i < size_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,has_missing_x,
                                 &missing_x,&missing_dx);
    }
    else {
/*
 * Point tmp_x to appropriate location in x.
 */
      tmp_x = &((double*)x)[index_x];
    }

    if(type_xmedian == NCL_double) tmp_xmedian = &((double*)xmedian)[i];
    if(type_xmrange == NCL_double) tmp_xmrange = &((double*)xmrange)[i];
    if(type_xrange == NCL_double)  tmp_xrange  = &((double*)xrange)[i];

    NGCALLF(dmedmrng,DMEDMRNG)(tmp_x,work,&inpts,&missing_dx.doubleval,
                               tmp_xmedian,tmp_xmrange,tmp_xrange,&nptused[i],&ier);

    if (ier == 2) {
      *tmp_xmedian = missing_dx.doubleval;
      *tmp_xmrange = missing_dx.doubleval;
      *tmp_xrange  = missing_dx.doubleval;
      ier_count++;
    }
    if(type_xmedian == NCL_float) {
      coerce_output_float_only(xmedian,tmp_xmedian,1,i);
    }
    if(type_xmrange  == NCL_float) {
      coerce_output_float_only(xmrange,tmp_xmrange,1,i);
    }
    if(type_xrange  == NCL_float) {
      coerce_output_float_only(xrange,tmp_xrange,1,i);
    }
    index_x += npts;
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"stat_medrng: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }

/*
 * Free unneeded memory.
 */
  if(type_x       != NCL_double) NclFree(tmp_x);
  if(type_xmedian != NCL_double) NclFree(tmp_xmedian);
  if(type_xmrange != NCL_double) NclFree(tmp_xmrange);
  if(type_xrange  != NCL_double) NclFree(tmp_xrange);
  NclFree(work);

  return(NhlNOERROR);
}


NhlErrorTypes dim_median_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmedian;
  double xrange, xmrange;
  double *tmp_xmedian = NULL;
  ng_size_t dsizes_median[NCL_MAX_DIMENSIONS];
  int nptused, ndims_median;
/*
 * various
 */
  ng_size_t i, l1, total_elements;
  int ier = 0, ier_count = 0;
  ng_size_t npts;
  int inpts;
  double *work;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Compute the total number of elements in output and input.
 */
  ndims_median = max(ndims_x-1,1);
  dsizes_median[0] = 1;

  total_elements = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_elements *= dsizes_x[i];
    dsizes_median[i] = dsizes_x[i];
  }    

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Allocate space for in/output arrays.
 */
  if(type_x != NCL_double) {
    xmedian     = (void*)calloc(total_elements,sizeof(float));
    tmp_x       = (double*)calloc(npts,sizeof(double));
    tmp_xmedian = (double*)calloc(1,sizeof(double));
    if(xmedian == NULL || tmp_xmedian == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    xmedian = (void*)calloc(total_elements,sizeof(double));
    if (xmedian == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'medmrng' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_elements; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x       = &((double*)x)[l1];
      tmp_xmedian = &((double*)xmedian)[i];
    }

    NGCALLF(dmedmrng,DMEDMRNG)(tmp_x,work,&inpts,&missing_dx.doubleval,
                               tmp_xmedian,&xmrange,&xrange,&nptused,&ier);

    if(type_x != NCL_double) ((float*)xmedian)[i] = (float)(*tmp_xmedian);

    l1 += npts;
    if (ier == 2) ier_count++;
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_median: %d rightmost sections of the input array contained all missing values",ier_count);
  }
/*
 * Free unneeded memory.
 */
  NclFree(work);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_xmedian);
  }
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * return double values
 */
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_median_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *dims;
  ng_size_t ndims;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *xmedian;
  double xrange, xmrange;
  double *tmp_xmedian = NULL;
  ng_size_t dsizes_median[NCL_MAX_DIMENSIONS];
  int nptused;
  int ndims_median;
/*
 * various
 */
  ;
  ng_size_t i, j;
  ng_size_t total_nl, total_nr, total_elements;
  ng_size_t npts;
  int inpts;
  int ier = 0, ier_count = 0;
  ng_size_t index_out, index_x, nrnx, index_nrx, index_nr;
  double *work;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: Invalid dimension sizes to do median across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to do the median across are "dims".
 */
  ndims_median = max(ndims_x-ndims,1);
  dsizes_median[0] = 1;

  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0];   i++) {
    total_nl *= dsizes_x[i];
    dsizes_median[i] = dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
    dsizes_median[i-ndims] = dsizes_x[i];
  }
  total_elements = total_nr * total_nl;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Allocate space for in/output arrays.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if(type_x != NCL_double) {
    xmedian     = (void*)calloc(total_elements,sizeof(float));
    tmp_xmedian = (double*)calloc(1,sizeof(double));
    if(xmedian == NULL || tmp_xmedian == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    xmedian = (void*)calloc(total_elements,sizeof(double));
    if (xmedian == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_median_n: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'medmrng' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
      index_out = index_nr + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      if(type_x == NCL_double) tmp_xmedian = &((double*)xmedian)[index_out];

      NGCALLF(dmedmrng,DMEDMRNG)(tmp_x,work,&inpts,&missing_dx.doubleval,
                                 tmp_xmedian,&xmrange,&xrange,&nptused,&ier);

      if(type_x != NCL_double) {
        ((float*)xmedian)[index_out] = (float)(*tmp_xmedian);
      }
      if (ier == 2) ier_count++;
    }
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_median_n: %d rightmost sections of the input array contained all missing values",ier_count);
  }
/*
 * Free unneeded memory.
 */
  NclFree(work);
  NclFree(tmp_x);
  if(type_x != NCL_double) NclFree(tmp_xmedian);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * return double values
 */
    return(NclReturnValue(xmedian,ndims_median,dsizes_median,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmvmean_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output
 */
  void *rmvmean;
/*
 * various
 */
  ng_size_t i, l1, total_size_x, total_size_x1;
  int ier = 0, ier_count = 0;
  ng_size_t npts;
  int inpts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Compute the total number of elements in output and input.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmean = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmean = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmean == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'rmvmean' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(drmvmean,DRMVMEAN)(tmp_x,&inpts,&missing_dx.doubleval,&ier);

    coerce_output_float_or_double(rmvmean,tmp_x,type_x,npts,l1);

    l1 += npts;
    if (ier == 2) ier_count++;
  }

  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmean: %d rightmost sections of the input array contained all missing values",ier_count);
  }

/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmvmean_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *dims;
  ng_size_t ndims;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output
 */
  void *rmvmean;
/*
 * various
 */
  ng_size_t i, j, index_x, nrnx, index_nrx;
  ng_size_t total_nl, total_nr, total_size_x, total_elements;
  int ier=0, ier_count=0;
  ng_size_t npts;
  int inpts;

/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean_n: Invalid dimension sizes to remove mean across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to remove the mean from are "dims".
 */
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  total_elements = total_nr * total_nl;
  total_size_x   = total_elements * npts;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmean = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmean = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmean == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmean_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'rmvmean' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(drmvmean,DRMVMEAN)(tmp_x,&inpts,&missing_dx.doubleval,&ier);

      coerce_output_float_or_double_step(rmvmean,tmp_x,type_x,npts,index_x,
                                         total_nr);
      if (ier == 2) ier_count++;
    }
  }

  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmean_n: %d rightmost sections of the input array contained all missing values",ier_count);
  }

/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmean,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmvmed_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *rmvmed;
  double *work;
/*
 * various
 */
  ng_size_t i, l1, total_size_x1, total_size_x;
  int ier=0, ier_count=0;
  ng_size_t npts;
  int inpts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Compute the total number of elements.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmed = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmed = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmed == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'rmvmed' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(drmvmed,DRMVMED)(tmp_x,work,&inpts,&missing_dx.doubleval,&ier);

    coerce_output_float_or_double(rmvmed,tmp_x,type_x,npts,l1);

    l1 += npts;
    if (ier == 2) ier_count++;
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmed: %d rightmost sections of the input array contained all missing values",ier_count);
  }

/*
 * Free work array.
 */
  NclFree(work);
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes dim_rmvmed_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *dims;
  ng_size_t ndims;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *rmvmed;
  double *work;
/*
 * various
 */
  ng_size_t i, j, index_x, nrnx, index_nrx, npts;
  ng_size_t total_nl, total_nr, total_size_x, total_elements;
  int ier=0, ier_count=0, inpts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: Invalid dimension sizes to remove median from, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to remove the median from are "dims".
 */
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  total_elements = total_nr * total_nl;
  total_size_x   = total_elements * npts;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    rmvmed = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    rmvmed = (void*)calloc(total_size_x,sizeof(double));
  }
  if( rmvmed == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for work array.
 */
  work = (double*)calloc(npts,sizeof(double));
  if (work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmvmed_n: Unable to allocate space for work array" );
    return(NhlFATAL);
  }
/*
 * Call the f77 double version of 'rmvmed' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(drmvmed,DRMVMED)(tmp_x,work,&inpts,&missing_dx.doubleval,&ier);

      coerce_output_float_or_double_step(rmvmed,tmp_x,type_x,npts,index_x,
                                         total_nr);
      if (ier == 2) ier_count++;
    }
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmvmed_n: %d rightmost sections of the input array contained all missing values",ier_count);
  }

/*
 * Free work array.
 */
  NclFree(work);
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(rmvmed,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes dim_standardize_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *opt;
/*
 * Output
 */
  void *standardize;
/*
 * various
 */
  ng_size_t i, l1, total_size_x1, total_size_x;
  int ier=0, ier_count=0;
  ng_size_t npts;
  int inpts;
/*
 * Retrieve parameter.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get second argument.
 */ 
  opt = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Compute the total number of elements minus the last dimension.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_x = total_size_x1 * npts;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    standardize = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    standardize = (void*)calloc(total_size_x,sizeof(double));
  }
  if( standardize == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'xstnd' with the full argument list.
 */
  l1 = 0;
  for(i = 0; i < total_size_x1; i++) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
    coerce_subset_input_double(x,tmp_x,l1,type_x,npts,0,NULL,NULL);

    NGCALLF(dxstnd,DXSTND)(tmp_x,&inpts,&missing_dx.doubleval,opt,&ier);

    coerce_output_float_or_double(standardize,tmp_x,type_x,npts,l1);

    l1 += npts;
    if (ier == 2) ier_count++;
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_standardize: %d rightmost sections of the input array contained all missing values",ier_count);
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_standardize_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *dims;
  ng_size_t ndims;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *opt;
/*
 * Output
 */
  void *standardize;
/*
 * various
 */
  ng_size_t i, j, index_x, nrnx, index_nrx, npts;
  ng_size_t total_nl, total_nr, total_size_x, total_elements;
  int ier=0, ier_count=0, inpts;

/*
 * Retrieve parameter.
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
/*
 * Get second argument.
 */ 
  opt = (int*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize_n: Invalid dimension sizes to do standardization across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to do the standardize across.
 */
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  total_elements = total_nr * total_nl;
  total_size_x   = total_elements * npts;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double no matter what, since we need a copy of the input
 * array.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array
 */
  if(type_x != NCL_double) {
    standardize = (void*)calloc(total_size_x,sizeof(float));
  }
  else {
    standardize = (void*)calloc(total_size_x,sizeof(double));
  }
  if( standardize == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_standardize_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'xstnd' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x = index_nrx + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(dxstnd,DXSTND)(tmp_x,&inpts,&missing_dx.doubleval,opt,&ier);

      coerce_output_float_or_double_step(standardize,tmp_x,type_x,npts,
                                         index_x,total_nr);
      if (ier == 2) ier_count++;
    }
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_standardize_n: %d rightmost sections of the input array contained all missing values",ier_count);
  }
/*
 * Free temp array.
 */
  NclFree(tmp_x);
/*
 * Return float if input isn't double, otherwise return double.
 */
  if(type_x != NCL_double) {
/*
 * Return float values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue(standardize,ndims_x,dsizes_x,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes dim_rmsd_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_y, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
/*
 * Output array variables
 */
  void *rmsd;
  double *tmp_rmsd = NULL;
  ng_size_t dsizes_rmsd[NCL_MAX_DIMENSIONS];
  int nptused, ndims_rmsd;
  NclScalar missing_rmsd;
  NclBasicDataTypes type_rmsd;
/*
 * various
 */
  ng_size_t i, total_elements;
  int ier = 0, ier_count = 0;
  ng_size_t npts, index_xy;
  int inpts;
/*
 * Retrieve parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
/*
 * x and y must be the same size.
 */
  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: x and y must have the same dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: x and y must have the same dimension sizes");
    return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 */
  ndims_rmsd = max(ndims_x-1,1);
  dsizes_rmsd[0] = 1;

  total_elements = 1;
  for(i = 0; i < ndims_x-1; i++) {
    total_elements *= dsizes_x[i];
    dsizes_rmsd[i] = dsizes_x[i];
  }    

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rmsd    = NCL_float;
    missing_rmsd = missing_rx;
    rmsd         = (void*)calloc(total_elements,sizeof(float));
    tmp_rmsd     = (double*)calloc(1,sizeof(double));
    if(rmsd == NULL || tmp_rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_rmsd    = NCL_double;
    missing_rmsd = missing_dx;
    rmsd         = (void*)calloc(total_elements,sizeof(double));
    if (rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Allocate space for coercing input arrays to double, if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 double version of 'rmsd' with the full argument list.
 */
  index_xy = 0;
  for(i = 0; i < total_elements; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_xy,type_x,npts,
                                 has_missing_x,&missing_x,&missing_dx);
    }
    else {
      tmp_x = &((double*)x)[index_xy];
    }

    if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
      coerce_subset_input_double(y,tmp_y,index_xy,type_y,npts,
                                 has_missing_y,&missing_y,&missing_dy);
    }
    else {
      tmp_y = &((double*)y)[index_xy];
    }

    if(type_rmsd == NCL_double) {
      tmp_rmsd = &((double*)rmsd)[i];
    }

    NGCALLF(drmsd,DRMSD)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                         &missing_dy.doubleval,tmp_rmsd,&nptused,&ier);

    if(type_rmsd != NCL_double) ((float*)rmsd)[i] = (float)(*tmp_rmsd);

    index_xy += npts;
    if (ier == 2) ier_count++;
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmsd: %d rightmost sections of one or both of the input arrays contained all missing values",ier_count);
  }
/*
 * Free unneeded memory.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_rmsd != NCL_double) NclFree(tmp_rmsd);

/*
 * Return.
 */
  if(has_missing_x || has_missing_y) {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,&missing_rmsd,
                          type_rmsd,0));
  }
  else {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,NULL,type_rmsd,0));
  }
}


NhlErrorTypes dim_rmsd_n_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_y, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
/*
 * Output array variables
 */
  void *rmsd;
  int *dims;
  ng_size_t ndims;
  double *tmp_rmsd = NULL;
  ng_size_t dsizes_rmsd[NCL_MAX_DIMENSIONS];
  int nptused, ndims_rmsd;
  NclScalar missing_rmsd;
  NclBasicDataTypes type_rmsd;
/*
 * various
 */
  ng_size_t i, j, index_out, index_xy, nrnx, index_nrx, index_nr;
  ng_size_t total_nl, total_nr, total_elements;
  int ier = 0, ier_count = 0;
  ng_size_t npts;
  int inpts;
/*
 * Retrieve parameters.
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
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
/*
 * x and y must be the same size.
 */
  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: x and y must have the same dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_x; i++ ) {
    if(dsizes_x[i] != dsizes_y[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: x and y must have the same dimension sizes");
    return(NhlFATAL);
    }
  }

  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: Invalid dimension sizes to calculate root-mean-square-difference across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to do the rmsd across are "dims".
 */
  ndims_rmsd = max(ndims_x-ndims,1);
  dsizes_rmsd[0] = 1;

  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0];   i++) {
    total_nl *= dsizes_x[i];
    dsizes_rmsd[i] = dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
    dsizes_rmsd[i-ndims] = dsizes_x[i];
  }
  total_elements = total_nr * total_nl;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Allocate space for output array.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rmsd    = NCL_float;
    missing_rmsd = missing_rx;
    rmsd         = (void*)calloc(total_elements,sizeof(float));
    tmp_rmsd     = (double*)calloc(1,sizeof(double));
    if(rmsd == NULL || tmp_rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_rmsd    = NCL_double;
    missing_rmsd = missing_dx;
    rmsd         = (void*)calloc(total_elements,sizeof(double));
    if (rmsd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: Unable to allocate memory for output array" );
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for coercing input arrays to double, if necessary.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  tmp_y = (double*)calloc(npts,sizeof(double));
  if(tmp_x == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_rmsd_n: Unable to allocate memory for coercing input arrays to double");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'rmsd' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_out = index_nr + j;
      index_xy  = index_nrx + j;
/*
 * Coerce subsection of x/y (tmp_x/tmp_y) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_xy,total_nr,type_x,npts,
                                      has_missing_x,&missing_x,&missing_dx);
      coerce_subset_input_double_step(y,tmp_y,index_xy,total_nr,type_y,npts,
                                      has_missing_y,&missing_y,&missing_dy);
      
      if(type_rmsd == NCL_double) tmp_rmsd = &((double*)rmsd)[index_out];

      NGCALLF(drmsd,DRMSD)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                           &missing_dy.doubleval,tmp_rmsd,&nptused,&ier);

      if(type_rmsd != NCL_double) {
        ((float*)rmsd)[index_out] = (float)(*tmp_rmsd);
      }
      if (ier == 2) ier_count++;
    }
  }
  if (ier_count) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_rmsd_n: %d rightmost sections of one or both of the input arrays contained all missing values",ier_count);
  }
/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);
  if(type_rmsd != NCL_double) NclFree(tmp_rmsd);

/*
 * Return.
 */
  if(has_missing_x || has_missing_y) {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,&missing_rmsd,
                          type_rmsd,0));
  }
  else {
    return(NclReturnValue(rmsd,ndims_rmsd,dsizes_rmsd,NULL,type_rmsd,0));
  }
}


NhlErrorTypes esacr_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *acr;
  double *tmp_acr = NULL;
  double *tmp_acv;
  ng_size_t *dsizes_acr;
/*
 * various
 */
  ng_size_t i, index_x, index_acr, total_size_x1, total_size_acr;
  int ier = 0, ier_count2 = 0, ier_count5 = 0;
  ng_size_t npts;
  int inpts, ret;
  double xmean, xvar;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get second argument.
 */
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: npts must be >= 2");
    return(NhlFATAL);
  }
/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
/* 
 * Get size of output variables.
 */
  total_size_acr = total_size_x1*mxlag1;
  dsizes_acr = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
  if (dsizes_acr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
  dsizes_acr[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acr[i] = dsizes_x[i];
  dsizes_acr[ndims_x-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    acr     = (void*)calloc(total_size_acr,sizeof(float));
    tmp_x   = (double*)calloc(npts,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acr == NULL || tmp_acr == NULL || tmp_acv == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    acr     = (void*)calloc(total_size_acr,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acr == NULL || tmp_acv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  index_x = index_acr = 0;
    
  for(i = 0; i < total_size_x1; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
      tmp_acr = &((double*)acr)[index_acr];
    }

    xvar = xmean = missing_dx.doubleval;

    NGCALLF(desauto,DESAUTO)(tmp_x,&inpts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,tmp_acv,tmp_acr,&ier);

    if (ier == -2) ier_count2++;
    if (ier == -5) ier_count5++;

    if(type_x != NCL_double) {
      coerce_output_float_only(acr,tmp_acr,mxlag1,index_acr);
    }

    index_x   += npts;
    index_acr += mxlag1;
  }
/*
 * Check errors.
 */
  if (ier_count2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacr: %d rightmost sections of the input array contained all missing values",ier_count2);
  }
  if (ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacr: the sample variance was zero for %d input array(s).\nAll values of a series are constant.",ier_count5);
  }
/*
 * free memory.
 */
  NclFree(tmp_acv);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_acr);
  }
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(acr,ndims_x,dsizes_acr,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(acr,ndims_x,dsizes_acr,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_acr);
  return(ret);
}

NhlErrorTypes esacv_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *acv;
  double *tmp_acr = NULL;
  double *tmp_acv = NULL;
  ng_size_t *dsizes_acv;
/*
 * various
 */
  ng_size_t i, index_x, index_acv, total_size_x1, total_size_acv;
  int ier = 0, ier_count2 = 0, ier_count5 = 0;
  ng_size_t npts;
  int inpts, ret;
  double xmean, xvar;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get second argument.
 */
  mxlag = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: npts must be >= 2");
    return(NhlFATAL);
  }
/*  
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x array.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];
/* 
 * Get size of output variables.
 */
  total_size_acv = total_size_x1*mxlag1;
  dsizes_acv = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
  if (dsizes_acv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
  dsizes_acv[0] = 1;
  for( i = 0; i < ndims_x-1; i++ ) dsizes_acv[i] = dsizes_x[i];
  dsizes_acv[ndims_x-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    acv     = (void*)calloc(total_size_acv,sizeof(float));
    tmp_x   = (double*)calloc(npts,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    tmp_acv = (double*)calloc(mxlag1,sizeof(double));
    if(acv == NULL || tmp_acr == NULL || tmp_acv == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  }
  else {
    acv     = (void*)calloc(total_size_acv,sizeof(double));
    tmp_acr = (double*)calloc(mxlag1,sizeof(double));
    if(acv == NULL || tmp_acr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esacv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'desauto' with the full argument list.
 */
  index_x = index_acv = 0;

  for(i = 0; i < total_size_x1; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
      tmp_acv = &((double*)acv)[index_acv];
    }

    xvar = xmean = missing_dx.doubleval;

    NGCALLF(desauto,DESAUTO)(tmp_x,&inpts,&missing_dx.doubleval,&xmean,
                             &xvar,mxlag,tmp_acv,tmp_acr,&ier);

    if (ier == -2) ier_count2++;
    if (ier == -5) ier_count5++;

    if(type_x != NCL_double) {
      coerce_output_float_only(acv,tmp_acv,mxlag1,index_acv);
    }

    index_x   += npts;
    index_acv += mxlag1;
  }
/*
 * Check errors.
 */
  if (ier_count2) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacv: %d rightmost sections of the input array contained all missing values",ier_count2);
  }
  if (ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esacv: the sample variance was zero for %d input array(s).\nAll values of a series are constant.",ier_count5);
  }
/*
 * free memory.
 */
  NclFree(tmp_acr);
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_acv);
  }

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(acv,ndims_x,dsizes_acv,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(acv,ndims_x,dsizes_acv,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_acv);
  return(ret);
}


NhlErrorTypes esccr_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *ccr;
  double *tmp_ccr = NULL;
  double *tmp_ccv;
  int ndims_ccr;
  ng_size_t *dsizes_ccr;
  NclBasicDataTypes type_ccr;
/*
 * various
 */
  ng_size_t i, j, index_x, index_y, index_ccr;
  ng_size_t total_size_x1, total_size_y1;
  ng_size_t total_size_ccr;
  int ier = 0, ier_count;
  ng_size_t npts, dimsizes_same;
  int inpts, ret;
  double xmean, xsd, ymean, ysd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  mxlag = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: the rightmost dimension of x and y must be the same");
      return(NhlFATAL);
  }
/*
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128 x (mxlag+1),
 * and NOT 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = 0;
        break;
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*      
 * Calculate size of input/output values.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: npts must be >= 2");
    return(NhlFATAL);
  }
/*  
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x and y arrays.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  total_size_y1 = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
/* 
 * Get size of output variables.
 */
  if(dimsizes_same) {
    ndims_ccr = ndims_x;
    total_size_ccr = total_size_x1 * mxlag1;
  }
  else {
    ndims_ccr = ndims_x + ndims_y - 1;
    total_size_ccr = total_size_x1 * total_size_y1 * mxlag1;
  }
  dsizes_ccr = (ng_size_t*)calloc(ndims_ccr,sizeof(ng_size_t));
  if (dsizes_ccr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }

/*
 * Calculate dimensions for ccr. If dimension sizes are *not* the
 * same, and input is:
 *
 *    x(npts)     and y(npts)   ==> ccr(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccr(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccr(k,mxlag+1)
 *
 *  etc.
 * 
 * If dimension sizes *are* the same, then you'll get the following:
 *
 *    x(m,n,npts) and y(m,n,npts) ==> ccr(m,n,mxlag+1)
 *    x(k,m,n,npts) and y(k,m,n,npts) ==> ccr(k,m,n,mxlag+1)
 *
 */
  if(dimsizes_same) {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
  }
  else {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccr[i] = dsizes_x[i];
    for( i = 0; i < ndims_y-1; i++ ) dsizes_ccr[ndims_x-1+i] = dsizes_y[i];
  }

  dsizes_ccr[ndims_ccr-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y   = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_ccr = NCL_float;
    ccr     = (void*)calloc(total_size_ccr,sizeof(float));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_ccr = NCL_double;
    ccr     = (void*)calloc(total_size_ccr,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  if(dimsizes_same) {
    index_x = index_ccr = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,npts,0,NULL,NULL);
      }
      else {
        tmp_y   = &((double*)y)[index_x];
      }

      if(type_ccr == NCL_double) {
        tmp_ccr = &((double*)ccr)[index_ccr];
      }
      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;

      NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);

      if(type_ccr != NCL_double) {
        coerce_output_float_only(ccr,tmp_ccr,mxlag1,index_ccr);
      }

      index_ccr += mxlag1;
      index_x   += npts;
      if(ier < 0) ier_count++;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
    }
  }
  else {
    index_x = index_ccr = 0;
    ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      index_y = 0;
      for(j = 1; j <= total_size_y1; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
        }
        else {
          tmp_y   = &((double*)y)[index_y];
        }

        if(type_ccr == NCL_double) {
          tmp_ccr = &((double*)ccr)[index_ccr];
        }
        xmean = xsd = missing_dx.doubleval;
        ymean = ysd = missing_dy.doubleval;

        NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&xmean,&ymean,&xsd,
                                 &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);

        if(type_ccr != NCL_double) {
          coerce_output_float_only(ccr,tmp_ccr,mxlag1,index_ccr);
        }
        index_y   += npts;
        index_ccr += mxlag1;
        if(ier < 0) ier_count++;
      }
      index_x += npts;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr: Non-fatal conditions encountered: all missing or constant values");
    }
  }
/*
 * free memory.
 */
  NclFree(tmp_ccv);
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_ccr != NCL_double) NclFree(tmp_ccr);

/*
 * Return values. 
 */
  if(type_ccr != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ccr);
  return(ret);
}


NhlErrorTypes dim_num_W( void)
{
  logical *input_var;
  int ndims_input;
  ng_size_t dsizes_input[NCL_MAX_DIMENSIONS];
  void *dim_num;
  int ndims_num;
  ng_size_t *dsizes_num;
  int has_missing_input;
  NclScalar missing_input;
  ng_size_t i, j, ii, size_leftmost, size_last, ret;
/* 
 * Retrieve input from NCL script.
 */
  input_var = (logical*)NclGetArgValue(
           0,
           1,
           &ndims_input, 
           dsizes_input,
           &missing_input,
           &has_missing_input,
           NULL,
           DONT_CARE);
/*
 * Calculate the product of the dimension sizes for the first n-1 
 * dimensions (size_leftmost).
 */
  ndims_num     = max(ndims_input-1,1);
  dsizes_num    = (ng_size_t*)calloc(ndims_num, sizeof(ng_size_t));
  dsizes_num[0] = 1;
  size_leftmost = 1;
  for(i = 0; i < ndims_input-1; i++ ) {
    dsizes_num[i]  = dsizes_input[i];
    size_leftmost *= dsizes_input[i];
  }
  size_last = dsizes_input[ndims_input-1];

/*
 * Allocate space for output (out_val).
 */
  dim_num = (void*)calloc(size_leftmost, sizeof(int));

  if(dim_num == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_num: Unable to allocate memory for output");
    return(NhlFATAL);
  }
/*
 * Loop through leftmost elements, and count the number of rightmost
 * elements that are True.
 */
  if(has_missing_input) {
    for(i = 0; i < size_leftmost; i++) {
      ii = i * size_last;
      ((int*)dim_num)[i] = 0;
      for(j = 0; j < size_last; j++) {
        if(input_var[ii+j] && input_var[ii+j] != missing_input.logicalval) {
          ((int*)dim_num)[i]++;
        }
      }
    }
  }
  else {
    for(i = 0; i < size_leftmost; i++) {
      ii = i * size_last;
      ((int*)dim_num)[i] = 0;
      for(j = 0; j < size_last; j++) {
        if((input_var[ii+j])) ((int*)dim_num)[i]++;
      }
    }
  }
  ret = NclReturnValue(dim_num, ndims_num, dsizes_num, NULL, NCL_int, 0);
  NclFree(dsizes_num);
  return(ret);
}


NhlErrorTypes dim_num_n_W( void)
{
  logical *input_var;
  int *dims;
  ng_size_t ndims;
  int ndims_input;
  ng_size_t dsizes_input[NCL_MAX_DIMENSIONS];
  void *dim_num;
  int ndims_num;
  ng_size_t *dsizes_num;
  int has_missing_input;
  NclScalar missing_input;
/*
 * various
 */
  ng_size_t i, j, k, index_out, index_in;
  ng_size_t total_nl, total_nr, total_elements, npts;
  int ret;

/* 
 * Retrieve input from NCL script.
 */
  input_var = (logical*)NclGetArgValue(
           0,
           2,
           &ndims_input, 
           dsizes_input,
           &missing_input,
           &has_missing_input,
           NULL,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_input) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_num_n: Invalid dimension sizes to do count across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_num_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to do the count across are "dims".
 */
  ndims_num     = max(ndims_input-ndims,1);
  dsizes_num    = (ng_size_t*)calloc(ndims_num, sizeof(ng_size_t));
  dsizes_num[0] = 1;
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0];   i++) {
    total_nl *= dsizes_input[i];
    dsizes_num[i] = dsizes_input[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_input[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_input; i++) {
    total_nr *= dsizes_input[i];
    dsizes_num[i-ndims] = dsizes_input[i];
  }
  total_elements = total_nr * total_nl;

/*
 * Allocate space for output (out_val).
 */
  dim_num = (void*)calloc(total_elements, sizeof(int));

  if(dim_num == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_num_n: Unable to allocate memory for output");
    return(NhlFATAL);
  }
/*
 * Loop through dimensions, and count the number of dims'
 * elements that are True.
 */
  if(has_missing_input) {
    for(i = 0; i < total_nl; i++) {
      for(j = 0; j < total_nr; j++) {
        index_out = (i*total_nr) + j;
        ((int*)dim_num)[index_out] = 0;
        for(k = 0; k < npts; k++) {
          index_in = (i*total_nr*npts) + (k*total_nr) + j;
          if(input_var[index_in] && 
             input_var[index_in] != missing_input.logicalval) {
            ((int*)dim_num)[index_out]++;
          }
        }
      }
    }
  }
  else {
    for(i = 0; i < total_nl; i++) {
      for(j = 0; j < total_nr; j++) {
        index_out = (i*total_nr) + j;
        ((int*)dim_num)[index_out] = 0;
        for(k = 0; k < npts; k++) {
          index_in = (i*total_nr*npts) + (k*total_nr) + j;
          if((input_var[index_in])) ((int*)dim_num)[index_out]++;
        }
      }
    }
  }
  ret = NclReturnValue(dim_num, ndims_num, dsizes_num, NULL, NCL_int, 0);
  NclFree(dsizes_num);
  return(ret);
}


NhlErrorTypes esccr_shields_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *ccr;
  double *tmp_ccr = NULL;
  double *tmp_ccv;
  int ndims_ccr;
  ng_size_t *dsizes_ccr;
  NclBasicDataTypes type_ccr;
/*
 * various
 */
  ng_size_t index_x, index_y, index_ccr;
  ng_size_t i, j, nc;
  ng_size_t total_size_x1, total_size_y1;
  ng_size_t total_size_ccr;
  int ier = 0, ier_count;
  ng_size_t npts, ncases;
  int inpts, ret;
  double xmean, xsd, ymean, ysd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  mxlag = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * The last dimension of x and y must be the same.
 */
  npts   = dsizes_x[ndims_x-1];
  ncases = dsizes_x[0];
  if( dsizes_y[ndims_y-1] != npts ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: the rightmost dimension of x and y must be the same");
      return(NhlFATAL);
  }

  if( dsizes_y[0] != ncases ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: the leftmost dimension of x and y must be the same");
      return(NhlFATAL);
  }

/*      
 * Calculate size of input/output values.
 */
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: npts must be >= 2");
    return(NhlFATAL);
  }
/*  
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x and y arrays.
 * Don't count the first dimension (ncases), because this is
 * a special dimension.
 */
  total_size_x1 = 1;
  for(i = 1; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  total_size_y1 = 1;
  for(i = 1; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
/* 
 * Get size of output variables.
 */
  ndims_ccr = ndims_x + ndims_y - 2;
  total_size_ccr = ncases * total_size_x1 * total_size_y1 * mxlag1;

  dsizes_ccr = (ng_size_t*)calloc(ndims_ccr,sizeof(ng_size_t));
  if (dsizes_ccr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }

/*
 * Calculate dimensions for ccr. If dimension sizes are *not* the
 * same, and input is:
 *
 *    x(nc,npts)     and y(nc,npts)   ==> ccr(nc,mxlag+1)
 *    x(nc,m,n,npts) and y(nc,k,npts) ==> ccr(nc,m,n,k,mxlag+1)
 *    x(nc,npts)     and y(nc,k,npts) ==> ccr(nc,k,mxlag+1)
 *
 *  etc.
 * 
 */
  dsizes_ccr[0] = ncases;
  for( i = 1; i <= ndims_x-2; i++ ) dsizes_ccr[i] = dsizes_x[i];
  for( i = 1; i <= ndims_y-2; i++ ) dsizes_ccr[ndims_x-2+i] = dsizes_y[i];

  dsizes_ccr[ndims_ccr-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y   = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_ccr = NCL_float;
    ccr     = (void*)calloc(total_size_ccr,sizeof(float));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_ccr = NCL_double;
    ccr     = (void*)calloc(total_size_ccr,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccr_shields: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  index_x = index_y = index_ccr = ier_count = 0;
  for(nc = 0; nc <= ncases-1; nc++) {
    index_x = nc * total_size_x1 * npts;
    for(i = 1; i <= total_size_x1; i++) {
      index_y = nc * total_size_y1 * npts;
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      for(j = 1; j <= total_size_y1; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
        }
        else {
          tmp_y   = &((double*)y)[index_y];
        }

        if(type_ccr == NCL_double) {
          tmp_ccr = &((double*)ccr)[index_ccr];
        }
        xmean = xsd = missing_dx.doubleval;
        ymean = ysd = missing_dy.doubleval;

        NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&xmean,&ymean,&xsd,
                                 &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);

        if(type_ccr != NCL_double) {
          coerce_output_float_only(ccr,tmp_ccr,mxlag1,index_ccr);
        }
        index_y   += npts;
        index_ccr += mxlag1;
        if(ier < 0) ier_count++;
      }
      index_x += npts;
    }
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"esccr_shields: Non-fatal conditions encountered: all missing or constant values");
  }
/*
 * free memory.
 */
  NclFree(tmp_ccv);
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_ccr != NCL_double) NclFree(tmp_ccr);

/*
 * Return values. 
 */
  if(type_ccr != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ccr,ndims_ccr,dsizes_ccr,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ccr);
  return(ret);
}


NhlErrorTypes esccv_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  int has_missing_y;
  NclScalar missing_x, missing_y;
  NclScalar missing_rx, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  int *mxlag, mxlag1;
/*
 * Output array variables
 */
  void *ccv;
  double *tmp_ccr;
  double *tmp_ccv = NULL;
  int ndims_ccv;
  ng_size_t *dsizes_ccv;
  NclBasicDataTypes type_ccv;
/*
 * various
 */
  ng_size_t index_x, index_y, index_ccv;
  ng_size_t i, j;
  ng_size_t total_size_x1, total_size_y1;
  ng_size_t total_size_ccv;
  int ier = 0, ier_count;
  ng_size_t npts, dimsizes_same;
  int inpts, ret;
  double xmean, xsd, ymean, ysd;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
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
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  mxlag = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: the rightmost dimension of x and y must be the same");
      return(NhlFATAL);
  }
/*
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128 x (mxlag+1),
 * and NOT 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = 0;
        break;
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*      
 * Check rightmost dimension.
 */
  npts = dsizes_x[ndims_x-1];
  if(npts < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: npts must be >= 2");
    return(NhlFATAL);
  }

/*  
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Check mxlag
 */
  if( *mxlag < 0 || *mxlag > npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: mxlag must be between 0 and npts");
    return(NhlFATAL);
  }
  mxlag1 = *mxlag + 1;
/*
 * Compute the total number of elements in our x and y arrays.
 */
  total_size_x1 = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_x1 *= dsizes_x[i];

  total_size_y1 = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_y1 *= dsizes_y[i];
/* 
 * Get size of output variables.
 */
  if(dimsizes_same) {
    ndims_ccv = ndims_x;
    total_size_ccv = total_size_x1 * mxlag1;
  }
  else {
    ndims_ccv = ndims_x + ndims_y - 1;
    total_size_ccv = total_size_x1 * total_size_y1 * mxlag1;
  }
  dsizes_ccv = (ng_size_t*)calloc(ndims_ccv,sizeof(ng_size_t));
  if (dsizes_ccv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate space for output arrays" );
    return(NhlFATAL);
  }
/*
 * Calculate dimensions for ccv. If dimension sizes are *not* the
 * same, and input is:
 *
 *    x(npts)     and y(npts)   ==> ccv(mxlag+1)
 *    x(m,n,npts) and y(k,npts) ==> ccv(m,n,k,mxlag+1)
 *    x(npts)     and y(k,npts) ==> ccv(k,mxlag+1)
 *
 *  etc.
 * 
 * If dimension sizes *are* the same, then you'll get the following:
 *
 *    x(m,n,npts) and y(m,n,npts) ==> ccv(m,n,mxlag+1)
 *    x(k,m,n,npts) and y(k,m,n,npts) ==> ccv(k,m,n,mxlag+1)
 *
 */
  if(dimsizes_same) {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
  }
  else {
    for( i = 0; i < ndims_x-1; i++ ) dsizes_ccv[i] = dsizes_x[i];
    for( i = 0; i < ndims_y-1; i++ ) dsizes_ccv[ndims_x-1+i] = dsizes_y[i];
  }

  dsizes_ccv[ndims_ccv-1] = mxlag1;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    tmp_x   = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_y != NCL_double) {
    tmp_y   = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for inputput array");
      return(NhlFATAL);
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_ccv = NCL_float;
    ccv     = (void*)calloc(total_size_ccv,sizeof(float));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    tmp_ccv = (double*)calloc(mxlag1,sizeof(double));
    if(ccv == NULL || tmp_ccr == NULL || tmp_ccv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_ccv = NCL_double;
    ccv     = (void*)calloc(total_size_ccv,sizeof(double));
    tmp_ccr = (double*)calloc(mxlag1,sizeof(double));
    if(ccv == NULL || tmp_ccr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"esccv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  if(dimsizes_same) {
    index_x = index_ccv = ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,npts,0,NULL,NULL);
      }
      else {
        tmp_y   = &((double*)y)[index_x];
      }

      if(type_ccv == NCL_double) {
        tmp_ccv = &((double*)ccv)[index_ccv];
      }

      xmean = xsd = missing_dx.doubleval;
      ymean = ysd = missing_dy.doubleval;

      NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                               &missing_dy.doubleval,&xmean,&ymean,&xsd,
                               &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);

      if(type_ccv != NCL_double) {
        coerce_output_float_only(ccv,tmp_ccv,mxlag1,index_ccv);
      }

      index_ccv += mxlag1;
      index_x   += npts;
      if(ier < 0) ier_count++;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
    }
  }
  else {
    index_x = index_ccv = ier_count = 0;
    for(i = 1; i <= total_size_x1; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
      }
      else {
        tmp_x   = &((double*)x)[index_x];
      }
      index_y = 0;
      for(j = 1; j <= total_size_y1; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,npts,0,NULL,NULL);
        }
        else {
          tmp_y   = &((double*)y)[index_y];
        }

        if(type_ccv == NCL_double) {
          tmp_ccv = &((double*)ccv)[index_ccv];
        }

        xmean = xsd = missing_dx.doubleval;
        ymean = ysd = missing_dy.doubleval;

        NGCALLF(descros,DESCROS)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&xmean,&ymean,&xsd,
                                 &ysd,mxlag,tmp_ccv,tmp_ccr,&ier);

        if(type_ccv != NCL_double) {
          coerce_output_float_only(ccv,tmp_ccv,mxlag1,index_ccv);
        }
        index_y   += npts;
        index_ccv += mxlag1;
        if(ier < 0) ier_count++;
      }
      index_x += npts;
    }
    if(ier_count > 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"esccv: Non-fatal conditions encountered: all missing or constant values");
    }
  }
/*
 * free memory.
 */
  NclFree(tmp_ccr);
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_ccv != NCL_double) NclFree(tmp_ccv);

/*
 * Return values. 
 */
  if(type_ccv != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(ccv,ndims_ccv,dsizes_ccv,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(ccv,ndims_ccv,dsizes_ccv,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_ccv);
  return(ret);
}

NhlErrorTypes dim_stat4_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *stat;
  double dxmean, dxvar, dxskew, dxkurt;
  int nptused;
  ng_size_t *dsizes_out;
/*
 * various
 */
  ng_size_t i, total_leftmost;
  int ier = 0;
  ng_size_t index_x, npts;
  int inpts;
  int ret, ier_count;
  double xsd;
  double *tmp_x = NULL;
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
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*  
 * Check input dimension size
 */
  npts = dsizes_x[ndims_x-1];
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Compute the total number of lefmost and rightmost elements in our x array.
 */
  total_leftmost = 1;
  for(i = 0; i < ndims_x-1; i++) total_leftmost *= dsizes_x[i];
/*
 * Calculate size of output arrays.
 */
  dsizes_out = (ng_size_t*)calloc(ndims_x,sizeof(ng_size_t));
  if(type_x == NCL_double) {
    stat = (void*)calloc(4*total_leftmost,sizeof(double));
    if(dsizes_out == NULL || stat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    tmp_x = (double*)calloc(npts,sizeof(double));
    stat  = (void*)calloc(4*total_leftmost,sizeof(float));
    if(dsizes_out == NULL || stat == NULL || tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * The output array will be 4 x (leftmost-1 dimensions of x)
 */
  dsizes_out[0] = 4;
  for(i = 1; i <= ndims_x-1; i++ ) dsizes_out[i] = dsizes_x[i-1];
/*
 * Loop across leftmost dimensions, and call the f77 version of 
 * 'dim_stat4' with the appropriate subset of 'x'.
 */
  index_x = ier_count = 0;
  for(i = 0; i < total_leftmost; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
      tmp_x   = &((double*)x)[index_x];
    }

    NGCALLF(dstat4,DSTAT4)(tmp_x,&inpts,&missing_dx.doubleval,&dxmean,&dxvar,
                           &xsd,&dxskew,&dxkurt,&nptused,&ier);

    if (ier == 2) {
/*
 * Input was all missing for this subset, so set output to missing
 * as well.
 */
      dxmean = missing_dx.doubleval;
      dxvar  = missing_dx.doubleval;
      dxskew = missing_dx.doubleval;
      dxkurt = missing_dx.doubleval;
      ier_count++;
    }
    coerce_output_float_or_double(stat,&dxmean,type_x,1,i);
    coerce_output_float_or_double(stat,&dxvar, type_x,1,i+total_leftmost);
    coerce_output_float_or_double(stat,&dxskew,type_x,1,i+total_leftmost*2); 
    coerce_output_float_or_double(stat,&dxkurt,type_x,1,i+total_leftmost*3);
    index_x += npts;
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_stat4: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }
/*
 * free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(stat,ndims_x,dsizes_out,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(stat,ndims_x,dsizes_out,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_out);
  return(ret);
}

NhlErrorTypes dim_stat4_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *dims;
  ng_size_t ndims;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output array variables
 */
  void *stat;
  double dxmean, dxvar, dxskew, dxkurt;
  int nptused;
  ng_size_t *dsizes_out;
/*
 * various
 */
  ng_size_t i, j, total_nl, total_nr, total_n, nrnx, index_nrx, index_nr;
  int ier = 0;
  ng_size_t index_x, index_out, npts;
  int inpts;
  int ret, ier_count;
  double xsd, *tmp_x;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  dims = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4_n: Invalid dimension sizes to calculate stats across, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4_n: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Compute the total number of leftmost and rightmost elements in our x array.
 */

  npts = total_nl = total_nr = 1;
  for(i = 0;       i < dims[0];   i++)       total_nl *= dsizes_x[i];
  for(i = 0; i < ndims ; i++)                npts     *= dsizes_x[dims[i]];
  for(i = dims[ndims-1]+1; i < ndims_x; i++) total_nr *= dsizes_x[i];
  total_n = total_nl * total_nr;

/*  
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Calculate size of output arrays.
 */
  dsizes_out = (ng_size_t*)calloc(ndims_x-ndims+1,sizeof(ng_size_t));
  tmp_x      = (double*)calloc(npts,sizeof(double));
  if(type_x == NCL_double) {
    stat = (void*)calloc(4*total_n,sizeof(double));
  }
  else {
    stat  = (void*)calloc(4*total_n,sizeof(float));
  }
  if(dsizes_out == NULL || stat == NULL || tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stat4_n: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * The output array will be 4 x (all but dims' dimensions of x)
 */
  dsizes_out[0] = 4;
  for(i = 0; i < dims[0]; i++ ) {
    dsizes_out[i+1] = dsizes_x[i];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++ ) {
    dsizes_out[i-ndims+1] = dsizes_x[i];
  }

/*
 * Loop across dimensions, and call the f77 version of 
 * 'dim_stat4_n' with the appropriate subset of 'x'.
 */
  ier_count = 0;
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_out = index_nr + j;
      index_x   = index_nrx + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(dstat4,DSTAT4)(tmp_x,&inpts,&missing_dx.doubleval,&dxmean,&dxvar,
                             &xsd,&dxskew,&dxkurt,&nptused,&ier);

      if (ier == 2) {
/*
 * Input was all missing for this subset, so set output to missing
 * as well.
 */
        dxmean = missing_dx.doubleval;
        dxvar  = missing_dx.doubleval;
        dxskew = missing_dx.doubleval;
        dxkurt = missing_dx.doubleval;
        ier_count++;
      }
      coerce_output_float_or_double(stat,&dxmean,type_x,1,index_out);
      coerce_output_float_or_double(stat,&dxvar, type_x,1,index_out+total_n);
      coerce_output_float_or_double(stat,&dxskew,type_x,1,index_out+total_n*2); 
      coerce_output_float_or_double(stat,&dxkurt,type_x,1,index_out+total_n*3);
    }
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_stat4_n: %d rightmost sections of the input array contained all missing values.\nOutput values set to missing in these sections",ier_count);
  }
/*
 * Free memory.
 */
  NclFree(tmp_x);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(stat,ndims_x,dsizes_out,&missing_rx,NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(stat,ndims_x,dsizes_out,&missing_dx,NCL_double,0);
  }
  NclFree(dsizes_out);
  return(ret);
}

NhlErrorTypes dim_acumrun_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  int *lrun, *opt, *dims;
  ng_size_t ndims;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
/*
 * Output
 */
  void *acr;
  double *tmp_acr;
  int nmsg;
  NclBasicDataTypes type_acr;
/*
 * various
 */
  ng_size_t i, j, index_x, nrnx, index_nrx;
  ng_size_t total_nl, total_nr, total_size_x, total_elements;
  ng_size_t npts;
  int inpts;

/*
 * Retrieve parameter.
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

  lrun = (int*)NclGetArgValue(
           1,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  opt = (int*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dims = (int*)NclGetArgValue(
           3,
           4,
           NULL,
           &ndims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  if(*lrun < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: 'lrun' must be >= 2");
    return(NhlFATAL);
  }

  if(*opt !=0  && *opt != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: 'opt' must be 0 or 1");
    return(NhlFATAL);
  }

/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: Invalid dimension sizes for accumulated sum dimension, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: Invalid dimension sizes for accumulated sum dimension, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total number of elements in output and input.
 *
 * The dimension(s) to do the sum across are "dims".
 */
  npts = total_nl = total_nr = total_elements = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    npts = npts*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
  }
  total_elements = total_nr * total_nl;
  total_size_x   = total_elements * npts;

/*
 * Check input dimension size
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce missing values, if any.
 */
  coerce_missing_more_types(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);

  tmp_x   = (double*)calloc(npts,sizeof(double));
  tmp_acr = (double*)calloc(npts,sizeof(double));
  if(type_x == NCL_double) {
    type_acr = NCL_double;
    acr = (void*)calloc(total_size_x,sizeof(double));
  }
  else if(type_x == NCL_float) {
    type_acr = NCL_float;
    acr = (void*)calloc(total_size_x,sizeof(float));
  }
  else if(type_x == NCL_long) {
    type_acr = NCL_long;
    acr = (void*)calloc(total_size_x,sizeof(long));
  }
  else {
    type_acr = NCL_int;
    acr = (void*)calloc(total_size_x,sizeof(int));
  }
  if( tmp_x == NULL || acr == NULL || tmp_acr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_acumrun_n: Unable to allocate memory for coercing arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Call the f77 double version of 'acr' with the full argument list.
 */
  nrnx = total_nr * npts;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    for(j = 0; j < total_nr; j++) {
      index_x   = index_nrx + j;
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      npts,0,NULL,NULL);

      NGCALLF(dacumrun,DACUMRUN)(tmp_x,&inpts,&missing_dx.doubleval,
                                 lrun,tmp_acr,&nmsg,opt);

      coerce_output_step(acr,tmp_acr,type_acr,npts,index_x,total_nr);
    }
  }

/*
 * Free temp array.
 */
  NclFree(tmp_acr);
  NclFree(tmp_x);
  return(NclReturnValue(acr,ndims_x,dsizes_x,&missing_rx,type_acr,0));
}



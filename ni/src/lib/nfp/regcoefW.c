#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dregcoef,DREGCOEF)(double *,double *,int *,double *,
                                       double *,double *,double *,int *,
                                       double *,double *,double *,double *,
                                       int *);

extern void  NGCALLF(dzregr1,DZREGR1)(int *,int *,int *,double *,double *,
                                      double *,double *,double *,double *,
                                      double *,double *,double *,double *,
                                      double *,double *,double *,double *,
                                      double *,double *);

NhlErrorTypes regcoef_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
  ng_size_t npts;
/*
 * Output array variables
 */
  void *tval, *rcoef;
  double *tmp_tval = NULL;
  double *tmp_rcoef = NULL;
  double xave, yave, rstd, yint;
  int ndims_tval;
  ng_size_t dsizes_tval[NCL_MAX_DIMENSIONS];
  int ndims_nptxy;
  ng_size_t dsizes_nptxy[NCL_MAX_DIMENSIONS];
  int ndims_rcoef;
  ng_size_t *dsizes_rcoef;
  NclBasicDataTypes type_tval, type_rcoef;
  int *nptxy;
/*
 * various
 */
  ng_size_t i, j, ly, lx, ln, dimsizes_same;
  ng_size_t total_size_leftmost_x, total_size_leftmost_y; 
  ng_size_t total_size_rcoef;
  int inpts, ier = 0, ier_count5 = 0, ier_count6 = 0, ret;
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
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
/*
 * Get output vars.
 */
  tval = (void*)NclGetArgValue(
           2,
           4,
           &ndims_tval, 
           dsizes_tval,
           NULL,
           NULL,
           &type_tval,
           1);
  nptxy = (int*)NclGetArgValue(
           3,
           4,
           &ndims_nptxy, 
           dsizes_nptxy,
           NULL,
           NULL,
           NULL,
           1);
/*
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The input array y must have as many or more dimensions than x");
    return(NhlFATAL);
  }
/*
 * Check the dimensions of x and y and see if they are the same.
 *
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128, and NOT
 * 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = 1;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = 0;
      }
    }
  }
  else {
    dimsizes_same = 0;
  }

/*
 * Get and check number of input points.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The rightmost dimension of x must be at least 2");
    return(NhlFATAL);
  }  

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];

/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_rcoef = max(1,ndims_x-1);
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    total_size_rcoef = total_size_leftmost_x;
    if(ndims_x == 1) {
      dsizes_rcoef[0] = 1;
    }
    else {
      for( i = 0; i < ndims_x-1; i++ ) {
        dsizes_rcoef[i] = dsizes_x[i];
      }
    }
  }
  else {
    total_size_rcoef = total_size_leftmost_x * total_size_leftmost_y;
    ndims_rcoef = max(1,ndims_x + ndims_y - 2);
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dsizes_rcoef[0] = 1;

    for( i = 0; i < ndims_x-1; i++ ) {
      dsizes_rcoef[i] = dsizes_x[i];
    }
    for( i = 0; i < ndims_y-1; i++ ) {
      dsizes_rcoef[ndims_x-1+i] = dsizes_y[i];
    }
  }
/*
 * dimension sizes of tval, nptxy must be the same as rcoef.
 */
  if( ndims_tval != ndims_rcoef || ndims_nptxy != ndims_rcoef ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The number of dimensions of tval and nptxy must be the same as rcoef");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_rcoef; i++ ) {
    if( dsizes_tval[i]  != dsizes_rcoef[i] || 
        dsizes_nptxy[i] != dsizes_rcoef[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The dimensions of tval and nptxy must be the same as rcoef");
      return(NhlFATAL);
    }
  }
/*
 * tval must be a float or double. It doesn't matter what the input type
 * is.
 */
  if(type_tval != NCL_float && type_tval != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: tval must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Allocate space for temporary x and y input.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for double precision tval. There's no need to do a
 * coercion because tval is an output-only variable (i.e, there are no
 * values coming in).  tval can only be float or double, so only allocate
 * space for a d.p. array if tval is float.
 */
  if(type_tval == NCL_float) {
    tmp_tval = (double*)calloc(1,sizeof(double));
    if( tmp_tval == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for coercing tval array to double precision");
      return(NhlFATAL);
    }
  }
/* 
 * Allocate size for output array (or scalar).
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rcoef = NCL_float;

    rcoef     = (float *)calloc(total_size_rcoef,sizeof(float));
    tmp_rcoef = (double *)calloc(1,sizeof(double));

    if(tmp_rcoef == NULL || rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output variable");
      return(NhlFATAL);
    }
  }
  else {
    type_rcoef = NCL_double;

    rcoef = (double *)calloc(total_size_rcoef,sizeof(double));
    if(rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output variable");
      return(NhlFATAL);
    }
  }
  if(dimsizes_same) {
/*
 * The x/y arrays are the same dimensionality, so loop through 
 * leftmost x and y at same time.
 */
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[lx];
      }

      if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,lx,type_y,npts,has_missing_y,
                                   &missing_y,&missing_dy);
      }
      else {
/*
 * Point tmp_y to appropriate locations in y.
 */
        tmp_y = &((double*)y)[lx];
      }

      if(type_tval  == NCL_double) tmp_tval  = &((double*)tval)[ln];
      if(type_rcoef == NCL_double) tmp_rcoef = &((double*)rcoef)[ln];

      NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,tmp_rcoef,tmp_tval,
                                 &nptxy[ln],&xave,&yave,&rstd,&yint,&ier);

      if (ier == 5) ier_count5++;
      if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
      if(type_tval  != NCL_double) ((float*)tval)[ln]  = (float)*tmp_tval;
      if(type_rcoef != NCL_double) ((float*)rcoef)[ln] = (float)*tmp_rcoef;

      lx += npts;
      ln ++;
    }
  }
  else {
/*
 * The x/y arrays are not the same dimensionality, so loop 
 * through leftmost x first, then leftmost y.
 */
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
        tmp_x  = &((double*)x)[lx];
      }
/*
 * Here's the leftmost y loop.
 */
      ly = 0;
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,ly,type_y,npts,has_missing_y,
                                     &missing_y,&missing_dy);
        }
        else {
          tmp_y  = &((double*)y)[ly];
        }
        
        if(type_tval  == NCL_double) tmp_tval  = &((double*)tval)[ln];
        if(type_rcoef == NCL_double) tmp_rcoef = &((double*)rcoef)[ln];
        
        NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                   &missing_dy.doubleval,
                                   tmp_rcoef,tmp_tval,&nptxy[ln],
                                   &xave,&yave,&rstd,&yint,&ier);

        if (ier == 5) ier_count5++;
        if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
        if(type_tval  != NCL_double) ((float*)tval)[ln]  = (float)*tmp_tval;
        if(type_rcoef != NCL_double) ((float*)rcoef)[ln] = (float)*tmp_rcoef;
        
        ly += npts;
        ln ++;
      }
      lx += npts;
    }
  }
/*
 * Handle error messages. 
 *
 * This was commented in 6.2.0, because it potentially
 * echoes a lot of output if you call this routine a lot.
 */
/*
  if(ier_count5 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regcoef: %d array(s) contained all missing values",ier_count5);
  }
  if (ier_count6 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regcoef: %d array(s) contained less than 3 non-missing values",ier_count6);
  }
 */
/*
 * free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_y     != NCL_double) NclFree(tmp_y);
  if(type_tval  != NCL_double) NclFree(tmp_tval);
  if(type_rcoef != NCL_double) NclFree(tmp_rcoef);

  if(type_rcoef != NCL_double) {
/*
 * Return float values with missing value set.
 */
    ret = NclReturnValue(rcoef,ndims_rcoef,dsizes_rcoef,&missing_ry,
                          NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(rcoef,ndims_rcoef,dsizes_rcoef,&missing_dy,
                          NCL_double,0);
  }
  NclFree(dsizes_rcoef);
  return(ret);
}


NhlErrorTypes regCoef_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
  ng_size_t npts;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  void *tval, *yint, *rstd, *rcoef;
  double tmp_tval, tmp_yint, tmp_rstd, tmp_rcoef;
  double xave, yave;
  int ndims_rcoef;
  ng_size_t *dsizes_rcoef;
  NclBasicDataTypes type_rcoef;
  int *nptxy;
/*
 * various
 */
  ng_size_t i, j, ly, lx, ln;
  logical dimsizes_same;
  ng_size_t total_size_leftmost_x, total_size_leftmost_y; 
  ng_size_t total_size_rcoef;
  int inpts, ier = 0, ier_count5 = 0, ier_count6 = 0;
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
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: The input array y must have as many or more dimensions than x");
    return(NhlFATAL);
  }
/*
 * Check the dimensions of x and y and see if they are the same.
 *
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128, and NOT
 * 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = True;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = False;
      }
    }
  }
  else {
    dimsizes_same = False;
  }

/*
 * Get and check number of input points.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: The rightmost dimension of x must be at least 2");
    return(NhlFATAL);
  }  

  if( dsizes_y[ndims_y-1] != npts ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: The rightmost dimension of x must be equal to the rightmost dimension of y");
    return(NhlFATAL);
  }  

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];

/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_rcoef  = max(1,ndims_x-1);
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    total_size_rcoef = total_size_leftmost_x;
    if(ndims_x == 1) {
      dsizes_rcoef[0] = 1;
    }
    else {
      for( i = 0; i < ndims_x-1; i++ ) {
        dsizes_rcoef[i] = dsizes_x[i];
      }
    }
  }
  else {
    total_size_rcoef = total_size_leftmost_x * total_size_leftmost_y;
    ndims_rcoef = max(1,ndims_x + ndims_y - 2);
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dsizes_rcoef[0] = 1;

    for( i = 0; i < ndims_x-1; i++ ) {
      dsizes_rcoef[i] = dsizes_x[i];
    }
    for( i = 0; i < ndims_y-1; i++ ) {
      dsizes_rcoef[ndims_x-1+i] = dsizes_y[i];
    }
  }
/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Allocate space for temporary x and y input.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

/* 
 * Allocate size for output arrays.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rcoef = NCL_float;

    rcoef = (float *)calloc(total_size_rcoef,sizeof(float));
    tval  = (float *)calloc(total_size_rcoef,sizeof(float));
    yint  = (float *)calloc(total_size_rcoef,sizeof(float));
    rstd  = (float *)calloc(total_size_rcoef,sizeof(float));
    nptxy = (int *)calloc(total_size_rcoef,sizeof(int));
  }
  else {
    type_rcoef = NCL_double;

    rcoef = (double *)calloc(total_size_rcoef,sizeof(double));
    tval  = (double *)calloc(total_size_rcoef,sizeof(double));
    yint  = (double *)calloc(total_size_rcoef,sizeof(double));
    rstd  = (double *)calloc(total_size_rcoef,sizeof(double));
    nptxy = (int *)calloc(total_size_rcoef,sizeof(int));

  }
  if(rcoef == NULL || tval == NULL || yint == NULL || rstd == NULL || nptxy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }
  if(dimsizes_same) {
/*
 * The x/y arrays are the same dimensionality, so loop through 
 * leftmost x and y at same time.
 */
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[lx];
      }

      if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,lx,type_y,npts,has_missing_y,
                                   &missing_y,&missing_dy);
      }
      else {
/*
 * Point tmp_y to appropriate locations in y.
 */
        tmp_y = &((double*)y)[lx];
      }

      NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,&tmp_rcoef,&tmp_tval,
                                 &nptxy[ln],&xave,&yave,&tmp_rstd,&tmp_yint,
                                 &ier);
      if (ier == 5) ier_count5++;
      if (ier == 6) ier_count6++;
/*
 * Coerce output to float or double.
 */
      if(type_rcoef != NCL_double) {
        ((float*)tval)[ln]  = (float)tmp_tval;
        ((float*)yint)[ln]  = (float)tmp_yint;
        ((float*)rstd)[ln]  = (float)tmp_rstd;
        ((float*)rcoef)[ln] = (float)tmp_rcoef;
      }
      else {
        ((double*)tval)[ln]  = tmp_tval;
        ((double*)yint)[ln]  = tmp_yint;
        ((double*)rstd)[ln]  = tmp_rstd;
        ((double*)rcoef)[ln] = tmp_rcoef;
      }
      lx += npts;
      ln ++;
    }
  }
  else {
/*
 * The x/y arrays are not the same dimensionality, so loop 
 * through leftmost x first, then leftmost y.
 */
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,has_missing_x,
                                   &missing_x,&missing_dx);
      }
      else {
        tmp_x  = &((double*)x)[lx];
      }
/*
 * Here's the leftmost y loop.
 */
      ly = 0;
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,ly,type_y,npts,has_missing_y,
                                     &missing_y,&missing_dy);
        }
        else {
          tmp_y  = &((double*)y)[ly];
        }
        
        NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                   &missing_dy.doubleval,&tmp_rcoef,&tmp_tval,
                                   &nptxy[ln],&xave,&yave,&tmp_rstd,&tmp_yint,
                                   &ier);
 
        if (ier == 5) ier_count5++;
        if (ier == 6) ier_count6++;
/*
 * Coerce output to float or double.
 */
        if(type_rcoef != NCL_double) {
          ((float*)tval)[ln]  = (float)tmp_tval;
          ((float*)yint)[ln]  = (float)tmp_yint;
          ((float*)rstd)[ln]  = (float)tmp_rstd;
          ((float*)rcoef)[ln] = (float)tmp_rcoef;
        }
        else {
          ((double*)tval)[ln]  = tmp_tval;
          ((double*)yint)[ln]  = tmp_yint;
          ((double*)rstd)[ln]  = tmp_rstd;
          ((double*)rcoef)[ln] = tmp_rcoef;
        }
        ly += npts;
        ln ++;
      }
      lx += npts;
    }
  }
/*
 * Handle error messages.
 *
 * This was commented in 6.2.0, because it potentially
 * echoes a lot of output if you call this routine a lot.
 */
/*
  if(ier_count5 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef: one or more arrays contained all missing values");
  }
  if (ier_count6 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef: one or more arrays contained less than 3 non-missing values");
  }
*/
/*
 * free memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  dsizes[0] = total_size_rcoef;
/*
 * Get ready to return everything.
 */
  if(type_rcoef == NCL_float) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_ry,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  else {
/* 
 * Either x and/or y are double, so return doubles.
 *
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_dy,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         nptxy,
                         NULL,
                         1,                    /*  ndims_rcoef,   */
                         dsizes,               /*  dsizes_rcoef,  */
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "nptxy",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Free memory.
 */
  NclFree(dsizes_rcoef);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


NhlErrorTypes regCoef_n_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
  ng_size_t npts;
/*
 * Arguments # 3 and 4
 */
  int *dims_nx, *dims_ny;
  ng_size_t dsizes_dims_nx[1], dsizes_dims_ny[1], ndims_n;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  void *tval, *yint, *rstd, *rcoef;
  double *tmp_tval = NULL;
  double *tmp_yint = NULL;
  double *tmp_rstd = NULL;
  double *tmp_rcoef = NULL;
  double xave, yave;
  int ndims_rcoef;
  ng_size_t *dsizes_rcoef;
  NclBasicDataTypes type_rcoef;
  int *nptxy;
/*
 * various
 */
  ng_size_t i, j, k, l, nrnx, nrny;
  ng_size_t index_x, index_y, index_rcoef, index_nrx, index_nry;
  logical dimsizes_same;
  ng_size_t size_leftmost_x, size_rightmost_x, size_rl_x; 
  ng_size_t size_leftmost_y, size_rightmost_y, size_rl_y; 
  ng_size_t total_size_rcoef;
  int inpts, ier = 0, ier_count5 = 0, ier_count6 = 0, nr;
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
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

/*
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: The input array y must have as many or more dimensions than x");
    return(NhlFATAL);
  }

/*
 * Get arguments #  and 4. These will be a 1D arrays of dimension indexes.
 */
  dims_nx = (int*)NclGetArgValue(
           2,
           4,
           NULL,
           dsizes_dims_nx,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  dims_ny = (int*)NclGetArgValue(
           3,
           4,
           NULL,
           dsizes_dims_ny,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Make sure dims_nx and dims_ny are valid dimensions. There are 
 * several tests here because this is a complicated situation.
 */
  if(dsizes_dims_nx[0] != dsizes_dims_ny[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: dims_nx and dims_ny must have the same number of indexes.");
    return(NhlFATAL);
  }
  ndims_n = dsizes_dims_nx[0];        /* "ndims_n" easier to remember! */

  if(ndims_n > ndims_x || ndims_n > ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: too many input dimensions were given for dims_nx and dims_ny");
    return(NhlFATAL);
  }

  npts = 1;
  for(i = 0; i < ndims_n; i++) {
    if (dims_nx[i] < 0 || dims_nx[i] >= ndims_x || 
        dims_ny[i] < 0 || dims_ny[i] >= ndims_y) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: One or more dims_nx and/or dims_ny dimension indexes are invalid");
      return(NhlFATAL);
    }
    if((i > 0) && (((dims_nx[i]-dims_nx[i-1]) != 1) || 
                    (dims_ny[i]-dims_ny[i-1]) != 1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: dims_nx/dims_ny dimension sizes must be consecutive and increasing");
      return(NhlFATAL);
    }

    if(dsizes_x[dims_nx[i]] != dsizes_y[dims_ny[i]]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: the dimension sizes indicated by dims_nx and dims_ny must match");
        return(NhlFATAL);
    }
    npts *= dsizes_x[dims_nx[i]];
  }
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: The number of points to calculate across must be at least 2");
    return(NhlFATAL);
  }  

/*
 * Check the dimensions of x and y and see if they are the same.
 *
 * If all the dimensions of x and y are the same, then we don't treat
 * the dimensions differently:  i.e. if x is 64 x 128 x 21 and y is
 * 64 x 128 x 21, then what gets returned will be 64 x 128, and NOT
 * 64 x 128 x 64 x 128 x (mxlag+1).
 */
  if(ndims_x == ndims_y) {
    dimsizes_same = True;
    for(i = 0; i < ndims_x; i++) {
      if(dsizes_x[i] != dsizes_y[i]) {
        dimsizes_same = False;
      }
    }
  }
  else {
    dimsizes_same = False;
  }

/*
 * Calculate size of leftmost and rightmost dimensions of x/y
 * and the number of input points for x and y. 
 */
  size_rightmost_x = size_leftmost_x = 1;
  size_rightmost_y = size_leftmost_y = 1;
  for( i = 0; i < dims_nx[0]; i++ ) {
    size_leftmost_x *= dsizes_x[i];
  }
  for( i = dims_nx[ndims_n-1]+1; i < ndims_x; i++) {
    size_rightmost_x *= dsizes_x[i];
  }
  if(!dimsizes_same) {
    for( i = 0; i < dims_ny[0]; i++ ) {
      size_leftmost_y *= dsizes_y[i];
    }
    for( i = dims_ny[ndims_n-1]+1; i < ndims_y; i++) {
      size_rightmost_y *= dsizes_y[i];
    }
    size_rl_y = size_leftmost_y * size_rightmost_y;
  }

  size_rl_x = size_leftmost_x * size_rightmost_x;

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;
  
/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_rcoef  = max(1,ndims_x-ndims_n);
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    total_size_rcoef = size_rl_x;
    if(ndims_x == 1) {
      dsizes_rcoef[0] = 1;
    }
    else {
      nr = 0;
      for( i = 0; i < dims_nx[0]; i++ ) {
        dsizes_rcoef[nr++] = dsizes_x[i];
      }
      for( i = dims_nx[ndims_n-1]+1; i < ndims_x; i++) {
        dsizes_rcoef[nr++] = dsizes_x[i];
      }
    }
  }
  else {
    total_size_rcoef = size_rl_x * size_rl_y;
    ndims_rcoef = max(1,ndims_x + ndims_y - (2*ndims_n));
    dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
    if(dsizes_rcoef == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    dsizes_rcoef[0] = 1;
    
    nr = 0;
    for( i = 0; i < dims_nx[0]; i++ ) {
      dsizes_rcoef[nr++] = dsizes_x[i];
    }
    for( i = dims_nx[ndims_n-1]+1; i < ndims_x; i++ ) {
      dsizes_rcoef[nr++] = dsizes_x[i];
    }
    for( i = 0; i < dims_ny[0]; i++ ) {
      dsizes_rcoef[nr++] = dsizes_y[i];
    }
    for( i = dims_ny[ndims_n-1]+1; i < ndims_y; i++ ) {
      dsizes_rcoef[nr++] = dsizes_y[i];
    }
  }
/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Allocate space for temporary x and y input no matter what.
 */
  tmp_x = (double*)calloc(npts,sizeof(double));
  tmp_y = (double*)calloc(npts,sizeof(double));
  if(tmp_x == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: Unable to allocate memory for coercing input arrays to double");
    return(NhlFATAL);
  }

/* 
 * Allocate size for output arrays.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rcoef = NCL_float;

    rcoef     = (float *)calloc(total_size_rcoef,sizeof(float));
    tval      = (float *)calloc(total_size_rcoef,sizeof(float));
    yint      = (float *)calloc(total_size_rcoef,sizeof(float));
    rstd      = (float *)calloc(total_size_rcoef,sizeof(float));
    nptxy     = (int *)calloc(total_size_rcoef,sizeof(int));
    tmp_tval  = (double*)calloc(1,sizeof(double));
    tmp_yint  = (double*)calloc(1,sizeof(double));
    tmp_rstd  = (double*)calloc(1,sizeof(double));
    tmp_rcoef = (double *)calloc(1,sizeof(double));

    if(tmp_rcoef == NULL || rcoef == NULL || nptxy == NULL ||
       tmp_tval  == NULL || tval  == NULL || tmp_yint == NULL ||
       yint == NULL || tmp_rstd  == NULL || rstd  == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
  else {
    type_rcoef = NCL_double;

    rcoef = (double *)calloc(total_size_rcoef,sizeof(double));
    tval  = (double *)calloc(total_size_rcoef,sizeof(double));
    yint  = (double *)calloc(total_size_rcoef,sizeof(double));
    rstd  = (double *)calloc(total_size_rcoef,sizeof(double));
    nptxy = (int *)calloc(total_size_rcoef,sizeof(int));

    if(rcoef == NULL || tval == NULL || yint == NULL || rstd == NULL || 
       nptxy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_n: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }

/*
 * This is the section that loops across the leftmost and rightmost
 * dimensions and calls the Fortran routine. There are two possible
 * looping situations: one where the x and y dimension sizes are the
 * same, (dimsizes_same=True), and one where they are not.
 */

  index_rcoef = 0;
  if(dimsizes_same) {
    nrnx = npts * size_rightmost_x;
    for(i = 0; i < size_leftmost_x; i++) {
      index_nrx = i*nrnx;
      for( j = 0; j < size_rightmost_x; j++ ) {
        index_x = index_nrx + j;

        coerce_subset_input_double_step(x,tmp_x,index_x,size_rightmost_x,type_x,npts,
                                        has_missing_x,&missing_x,&missing_dx);
        coerce_subset_input_double_step(y,tmp_y,index_x,size_rightmost_x,type_y,npts,
                                        has_missing_y,&missing_y,&missing_dy);
        if(type_rcoef == NCL_double) {
          tmp_tval  = &((double*)tval)[index_rcoef];
          tmp_yint  = &((double*)yint)[index_rcoef];
          tmp_rstd  = &((double*)rstd)[index_rcoef];
          tmp_rcoef = &((double*)rcoef)[index_rcoef];
        }

        NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                   &missing_dy.doubleval,tmp_rcoef,tmp_tval,
                                   &nptxy[index_rcoef],&xave,&yave,tmp_rstd,tmp_yint,
                                   &ier);

        if (ier == 5) ier_count5++;
        if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
        if(type_rcoef != NCL_double) {
          ((float*)tval)[index_rcoef]  = (float)*tmp_tval;
          ((float*)yint)[index_rcoef]  = (float)*tmp_yint;
          ((float*)rstd)[index_rcoef]  = (float)*tmp_rstd;
          ((float*)rcoef)[index_rcoef] = (float)*tmp_rcoef;
        }
      }
      index_rcoef ++;
    }
  }
  else {
    nrnx = npts * size_rightmost_x;
    nrny = npts * size_rightmost_y;
    for(i = 0; i < size_leftmost_x; i++) {
      index_nrx = i*nrnx;
      for( j = 0; j < size_rightmost_x; j++ ) {
        index_x = index_nrx + j;

        coerce_subset_input_double_step(x,tmp_x,index_x,size_rightmost_x,
                                        type_x,npts,has_missing_x,
                                        &missing_x,&missing_dx);
        for(k = 0; k < size_leftmost_y; k++) {
          index_nry = k*nrny;
          for( l = 0; l < size_rightmost_y; l++ ) {
            index_y = index_nry + l;

            coerce_subset_input_double_step(y,tmp_y,index_y,size_rightmost_y,
                                            type_y,npts,has_missing_y,
                                            &missing_y,&missing_dy);
            if(type_rcoef == NCL_double) {
              tmp_tval  = &((double*)tval)[index_rcoef];
              tmp_yint  = &((double*)yint)[index_rcoef];
              tmp_rstd  = &((double*)rstd)[index_rcoef];
              tmp_rcoef = &((double*)rcoef)[index_rcoef];
            }

            NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                       &missing_dy.doubleval,tmp_rcoef,tmp_tval,
                                       &nptxy[index_rcoef],&xave,&yave,tmp_rstd,tmp_yint,
                                       &ier);

            if (ier == 5) ier_count5++;
            if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
            if(type_rcoef != NCL_double) {
              ((float*)tval)[index_rcoef]  = (float)*tmp_tval;
              ((float*)yint)[index_rcoef]  = (float)*tmp_yint;
              ((float*)rstd)[index_rcoef]  = (float)*tmp_rstd;
              ((float*)rcoef)[index_rcoef] = (float)*tmp_rcoef;
            }
            index_rcoef ++;
          }
        }
      }
    }
  }

/*
 * Handle error messages.
 *
 * This was commented in 6.2.0, because it potentially
 * echoes a lot of output if you call this routine a lot.
 */
/*
  if(ier_count5 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef_n: %d array(s) contained all missing values",ier_count5);
  }
  if (ier_count6 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef_n: %d array(s) contained less than 3 non-missing values",ier_count6);
  }
 */
/*
 * free memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);
  if(type_rcoef != NCL_double) {
    NclFree(tmp_rcoef);
    NclFree(tmp_tval);
    NclFree(tmp_yint);
    NclFree(tmp_rstd);
  }

  dsizes[0] = total_size_rcoef;
/*
 * Get ready to return everything.
 */
  if(type_rcoef == NCL_float) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_ry,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  else {
/* 
 * Either x and/or y are double, so return doubles.
 *
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_dy,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         nptxy,
                         NULL,
                         1,                    /*  ndims_rcoef,   */
                         dsizes,               /*  dsizes_rcoef,  */
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "nptxy",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Free memory.
 */
  NclFree(dsizes_rcoef);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


NhlErrorTypes regCoef_shields_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x = NULL;
  double *tmp_y = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
  ng_size_t npts;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  void *tval, *yint, *rstd, *rcoef;
  double *tmp_tval = NULL;
  double *tmp_yint = NULL;
  double *tmp_rstd = NULL;
  double *tmp_rcoef = NULL;
  double xave, yave;
  int ndims_rcoef;
  ng_size_t *dsizes_rcoef;
  NclBasicDataTypes type_rcoef;
  int *nptxy;
/*
 * various
 */
  ng_size_t i, j, ly, lx, ln;
  ng_size_t total_size_leftmost_x, total_size_leftmost_y; 
  ng_size_t total_size_rcoef;
  int inpts, ier = 0, ier_count5 = 0, ier_count6 = 0;
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
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: The input array y must have as many or more dimensions than x");
    return(NhlFATAL);
  }

/*
 * Get and check number of input points.
 */
  npts = dsizes_x[1];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: The rightmost dimension of x must be at least 2");
    return(NhlFATAL);
  }  

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

  total_size_leftmost_x = dsizes_x[0];
/*
 * Don't count first dimension, since this should be the same as the
 * first dimension of x.
 */
  total_size_leftmost_y = 1;
  for(i = 1; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];

/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of y. 
 */
  total_size_rcoef = total_size_leftmost_y * dsizes_y[0];
  ndims_rcoef = ndims_y - 1;
  dsizes_rcoef = (ng_size_t*)calloc(ndims_rcoef,sizeof(ng_size_t));
  if(dsizes_rcoef == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_y-1; i++ ) {
    dsizes_rcoef[i] = dsizes_y[i];
  }

/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
/*
 * Allocate space for temporary x and y input.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(npts,sizeof(double));
    if(tmp_y == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: Unable to allocate memory for input array");
      return(NhlFATAL);
    }
  } 

/* 
 * Allocate size for output arrays.
 */
  if(type_x != NCL_double && type_y != NCL_double) {
    type_rcoef = NCL_float;

    rcoef     = (float *)calloc(total_size_rcoef,sizeof(float));
    tval      = (float *)calloc(total_size_rcoef,sizeof(float));
    yint      = (float *)calloc(total_size_rcoef,sizeof(float));
    rstd      = (float *)calloc(total_size_rcoef,sizeof(float));
    nptxy     = (int *)calloc(total_size_rcoef,sizeof(int));
    tmp_tval  = (double*)calloc(1,sizeof(double));
    tmp_yint  = (double*)calloc(1,sizeof(double));
    tmp_rstd  = (double*)calloc(1,sizeof(double));
    tmp_rcoef = (double *)calloc(1,sizeof(double));

    if(tmp_rcoef == NULL || rcoef == NULL || nptxy == NULL ||
       tmp_tval  == NULL || tval  == NULL || tmp_yint == NULL ||
       yint == NULL || tmp_rstd  == NULL || rstd  == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
  else {
    type_rcoef = NCL_double;

    rcoef = (double *)calloc(total_size_rcoef,sizeof(double));
    tval  = (double *)calloc(total_size_rcoef,sizeof(double));
    yint  = (double *)calloc(total_size_rcoef,sizeof(double));
    rstd  = (double *)calloc(total_size_rcoef,sizeof(double));
    nptxy = (int *)calloc(total_size_rcoef,sizeof(int));

    if(rcoef == NULL || tval == NULL || yint == NULL || rstd == NULL || 
       nptxy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef_shields: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'regCoef_shields' with the full argument list.
 */
  lx = ly = ln = 0;
  for(i = 1; i <= total_size_leftmost_x; i++) {
    if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,lx,type_x,npts,has_missing_x,
                 &missing_x,&missing_dx);
    }
    else {
      tmp_x  = &((double*)x)[lx];
    }
      
    for(j = 1; j <= total_size_leftmost_y; j++) {
      if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
    coerce_subset_input_double(y,tmp_y,ly,type_y,npts,has_missing_y,
                   &missing_y,&missing_dy);
      }
      else {
        tmp_y  = &((double*)y)[ly];
      }
      
      if(type_rcoef == NCL_double) {
        tmp_tval  = &((double*)tval)[ln];
        tmp_yint  = &((double*)yint)[ln];
        tmp_rstd  = &((double*)rstd)[ln];
        tmp_rcoef = &((double*)rcoef)[ln];
      }
      
      NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&inpts,&missing_dx.doubleval,
                                 &missing_dy.doubleval,tmp_rcoef,tmp_tval,
                                 &nptxy[ln],&xave,&yave,tmp_rstd,tmp_yint,
                                 &ier);

      if (ier == 5) ier_count5++;
      if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
      if(type_rcoef != NCL_double) {
        ((float*)tval)[ln]  = (float)*tmp_tval;
        ((float*)yint)[ln]  = (float)*tmp_yint;
        ((float*)rstd)[ln]  = (float)*tmp_rstd;
        ((float*)rcoef)[ln] = (float)*tmp_rcoef;
      }
      
      ly += npts;
      ln ++;
    }
    lx += npts;
  }
/*
 * Handle error messages.
 */
  if(ier_count5 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef_shields: %d array(s) contained all missing values",ier_count5);
  }
  if (ier_count6 > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef_shields: %d array(s) contained less than 3 non-missing values",ier_count6);
  }
/*
 * free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_y     != NCL_double) NclFree(tmp_y);
  if(type_rcoef != NCL_double) {
    NclFree(tmp_rcoef);
    NclFree(tmp_tval);
    NclFree(tmp_yint);
    NclFree(tmp_rstd);
  }

  dsizes[0] = total_size_rcoef;
/*
 * Get ready to return everything.
 */
  if(type_rcoef == NCL_float) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_ry,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  else {
/* 
 * Either x and/or y are double, so return doubles.
 *
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      rcoef,
                      &missing_dy,
                      ndims_rcoef,
                      dsizes_rcoef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,                    /*  ndims_rcoef,   */
                   dsizes,               /*  dsizes_rcoef,  */
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );
  }
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         nptxy,
                         NULL,
                         1,                    /*  ndims_rcoef,   */
                         dsizes,               /*  dsizes_rcoef,  */
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "nptxy",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Free memory.
 */
  NclFree(dsizes_rcoef);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


NhlErrorTypes regline_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *dx, *dy;
  ng_size_t dsizes_x[1];
  ng_size_t dsizes_y[1];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
  ng_size_t npts;
/*
 * Output array variables
 */
  double *rcoef, *tval, *rstd, *yint, *xave, *yave;
  float *rrcoef, *rtval, *ryint, *rrstd, *rxave, *ryave;
  int *nptxy, ier = 0;
  int inpts;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
/*
 * The x and y arrays coming in must have the same length.
 */
  if( dsizes_x[0] != dsizes_y[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The input arrays must be the same length");
    return(NhlFATAL);
  }
/*
 * Get and check number of input points.
 */
  npts = dsizes_x[0];
  if( npts < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The length of x and y must be at least 2");
    return(NhlFATAL);
  }  

/*
 * Test input dimension sizes.
 */
  if(npts > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: npts = %ld is greater than INT_MAX", npts);
    return(NhlFATAL);
  }
  inpts = (int) npts;

/*
 * Coerce x and y to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
  dx = coerce_input_double(x,type_x,dsizes_x[0],has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,dsizes_y[0],has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing x and y arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output variables.
 */
  rcoef = (double *)calloc(1,sizeof(double));
  tval  = (double *)calloc(1,sizeof(double));
  yint  = (double *)calloc(1,sizeof(double));
  rstd  = (double *)calloc(1,sizeof(double));
  xave  = (double *)calloc(1,sizeof(double));
  yave  = (double *)calloc(1,sizeof(double));
  nptxy =   (int *)calloc(1,sizeof(int));
  if( rcoef == NULL || tval == NULL || xave == NULL || yave == NULL ||
      nptxy == NULL || yint == NULL || rstd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for output values");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'regline' with the full argument list.
 */
   NGCALLF(dregcoef,DREGCOEF)(&dx[0],&dy[0],&inpts,&missing_dx.doubleval,
                              &missing_dy.doubleval,rcoef,tval,nptxy,xave,
                              yave,rstd,yint,&ier);

  if (ier == 5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regline: The x and/or y array contains all missing values");
  }
  if (ier == 6) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regline: The x and/or y array contains less than 3 non-missing values");
  }
/*
 * free memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

/*
 * Set up variable to return.
 */
  dsizes[0] = 1;

  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * None of the input is double, so return floats.
 *
 * Allocate space for coercing output to float.
 */
    rrcoef = (float *)calloc(1,sizeof(float));
    rtval  = (float *)calloc(1,sizeof(float));
    ryint  = (float *)calloc(1,sizeof(float));
    rrstd  = (float *)calloc(1,sizeof(float));
    rxave  = (float *)calloc(1,sizeof(float));
    ryave  = (float *)calloc(1,sizeof(float));
    if( rrcoef == NULL || rtval == NULL || rxave == NULL || ryave == NULL ||
        rrstd == NULL || ryint == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing output values back to floating point");
      return(NhlFATAL);
    }
/*
 * Coerce double to float.
 */
    *rrcoef = (float)*rcoef;
    *rtval  = (float)*tval;
    *ryint  = (float)*yint;
    *rrstd  = (float)*rstd;
    *rxave  = (float)*xave;
    *ryave  = (float)*yave;
/*
 * Free up variables holding double precision values.
 */
    NclFree(rcoef);
    NclFree(tval);
    NclFree(yint);
    NclFree(rstd);
    NclFree(xave);
    NclFree(yave);
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      (void*)rrcoef,
                      &missing_ry,
                      1,
                      dsizes,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rtval,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rrstd,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   nptxy,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypeintClass
                   );
    _NclAddAtt(
               att_id,
               "nptxy",
               att_md,
               NULL
               );
    
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rxave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "xave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   ryave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );

    _NclAddAtt(
               att_id,
               "yave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   ryint,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );
  }
  else {
/* 
 * Either x and/or y are double, so return doubles.
 *
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      (void*)rcoef,
                      &missing_dy,
                      1,
                      dsizes,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   tval,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "tval",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   rstd,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "rstd",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   nptxy,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypeintClass
                   );
    _NclAddAtt(
               att_id,
               "nptxy",
               att_md,
               NULL
               );
    
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   xave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "xave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yave,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yave",
               att_md,
               NULL
               );

    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   yint,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
    _NclAddAtt(
               att_id,
               "yintercept",
               att_md,
               NULL
               );
  }
  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


NhlErrorTypes reg_multlin_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  logical *opt;
  ng_size_t dsizes_y[1];
  ng_size_t dsizes_x[2];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_ry;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  double *cnorm, *resid, *tmp_constant;
  double *wk, *yy, *cov, *xsd, *xmean, *a, *ainv, *s;
  int impts, inpts, impts2;
/*
 * Output variables
 */
  void *coef, *constant;
  double *tmp_coef;
  ng_size_t dsizes_coef[1];
  NclBasicDataTypes type_coef;
  ng_size_t size_x, mpts, mpts2, npts;

/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  y = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

  x = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The y and x coming in must be 1D and 2D respectively. The rightmost
 * dimension of x must be the same as y's dimension.
 */
  if(dsizes_x[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reg_multlin: The rightmost dimension of x must be the same as the dimension of y");
    return(NhlFATAL);
  }  

/*
 * Get array sizes.
 */
  mpts           = dsizes_x[0];
  mpts2          = 2*mpts;
  npts           = dsizes_x[1];
  size_x         = mpts * npts;
  dsizes_coef[0] = mpts;

/*
 * Test input dimension sizes.
 */
  if((mpts > INT_MAX) || (npts > INT_MAX) || (mpts2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reg_multlin: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  impts  = (int) mpts;
  impts2 = (int) mpts2;
  inpts  = (int) npts;

/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);

/*
 * Coerce x and y to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,size_x,has_missing_x,&missing_x,
                              &missing_dx);
  tmp_y = coerce_input_double(y,type_y,npts,has_missing_y,&missing_y,
                              &missing_dy);

  if(tmp_x == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reg_multlin: Unable to coerce input variables to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for other variables.
 */
  cnorm        = (double*)calloc(mpts,sizeof(double));
  resid        = (double*)calloc(npts,sizeof(double));
  tmp_constant = (double*)calloc(1,sizeof(double));
/*
 * These were all adjustable arrays in the Fortran routine that
 * caused problems on some systems and had to be allocated here.
 */
  wk           = (double*)calloc(mpts*mpts2,sizeof(double));
  yy           = (double*)calloc(npts,sizeof(double));
  cov          = (double*)calloc(mpts*mpts,sizeof(double));
  xsd          = (double*)calloc(mpts,sizeof(double));
  xmean        = (double*)calloc(mpts,sizeof(double));
  a            = (double*)calloc(mpts*mpts,sizeof(double));
  ainv         = (double*)calloc(mpts*mpts,sizeof(double));
  s            = (double*)calloc(mpts,sizeof(double));
  if(cnorm == NULL || resid == NULL || tmp_constant == NULL || wk == NULL ||
         yy == NULL || cov == NULL || xsd == NULL || xmean == NULL || a == NULL ||
         ainv == NULL || s == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reg_multlin: Unable to allocate memory for input arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate size for output array
 */
  if(type_x == NCL_double || type_y == NCL_double) {
    type_coef = NCL_double;
    coef      = (double *)calloc(mpts,sizeof(double));
    constant  = (double *)calloc(1,sizeof(double));
  }
  else {
    type_coef = NCL_float;
    coef      = (float *)calloc(mpts,sizeof(float));
    constant  = (float *)calloc(1,sizeof(float));
  }
  tmp_coef = coerce_output_double(coef,type_coef,mpts);
  if(coef == NULL || tmp_coef == NULL || constant == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"reg_multlin: Unable to allocate memory for output variable");
    return(NhlFATAL);
  }

  NGCALLF(dzregr1,DZREGR1)(&inpts,&impts,&impts2,tmp_y,&missing_dy.doubleval,
                           tmp_x,&missing_dx.doubleval,tmp_coef,resid,
                           tmp_constant,cnorm,wk,yy,cov,xsd,xmean,a,ainv,s);

/*
 * Coerce tmp_constant scalar to appropriate type.
 */
  coerce_output_float_or_double(constant,tmp_constant,type_coef,1,0);

/*
 * Free up memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  NclFree(cnorm);
  NclFree(resid);
  NclFree(tmp_constant);
  NclFree(wk);
  NclFree(yy);
  NclFree(cov);
  NclFree(xsd);
  NclFree(xmean);
  NclFree(a);
  NclFree(ainv);
  NclFree(s);
  
/*
 * Get ready to return the data and add a "constant" attribute.
 */
  if(type_coef == NCL_float) {
    coerce_output_float_only(coef,tmp_coef,mpts,0);
    NclFree(tmp_coef);

/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      coef,
                      &missing_ry,
                      1,
                      dsizes_coef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypefloatClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   constant,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypefloatClass
                   );
  }
  else {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                      NULL,
                      NULL,
                      Ncl_MultiDValData,
                      0,
                      coef,
                      &missing_dy,
                      1,
                      dsizes_coef,
                      TEMPORARY,
                      NULL,
                      (NclObjClass)nclTypedoubleClass
                      );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                   NULL,
                   NULL,
                   Ncl_MultiDValData,
                   0,
                   constant,
                   NULL,
                   1,
                   dsizes,
                   TEMPORARY,
                   NULL,
                   (NclObjClass)nclTypedoubleClass
                   );
  }

  _NclAddAtt(
             att_id,
             "constant",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);

  return(NhlNOERROR);
}

#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include "wrapper.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dregcoef,DREGCOEF)(double *,double *,int *,double *,
                                       double *,double *,int *,double *,
                                       double *,int *);

NhlErrorTypes regcoef_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  void *tval, *rcoef;
  double *tmp_tval, *tmp_rcoef, xave, yave;
  int ndims_tval, dsizes_tval[NCL_MAX_DIMENSIONS];
  int ndims_nptxy, dsizes_nptxy[NCL_MAX_DIMENSIONS];
  int ndims_rcoef, *dsizes_rcoef;
  NclBasicDataTypes type_tval, type_rcoef;
  int *nptxy;
/*
 * various
 */
  int i, j, k, ly, lx, ln, dimsizes_same;
  int total_size_leftmost_x, total_size_leftmost_y; 
  int total_size_x, total_size_y, total_size_rcoef;
  int ier = 0, ier_count5 = 0, ier_count6 = 0;
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
           2);
  y = (void*)NclGetArgValue(
           1,
           4,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
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
  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];
  total_size_x = total_size_leftmost_x * npts;

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];
  total_size_y = total_size_leftmost_y * npts;

/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_rcoef = max(1,ndims_x-1);
    dsizes_rcoef = (int*)calloc(ndims_rcoef,sizeof(int));
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
    dsizes_rcoef = (int*)calloc(ndims_rcoef,sizeof(int));
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
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  if(missing_dx.doubleval != missing_dy.doubleval) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and y missing values must be the same");
    return(NhlFATAL);
  }
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
/*
 * Call the f77 version of 'regcoef' with the full argument list.
 */
  if(dimsizes_same) {
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,0,
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
        coerce_subset_input_double(y,tmp_y,lx,type_y,npts,0,
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

      NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                 tmp_rcoef,tmp_tval,&nptxy[ln],
                                 &xave,&yave,&ier);
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
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      ly = 0;
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
          coerce_subset_input_double(x,tmp_x,lx,type_x,npts,0,
                                     &missing_x,&missing_dx);
        }
        else {
          tmp_x  = &((double*)x)[lx];
        }
        
        if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,ly,type_y,npts,0,
                                     &missing_y,&missing_dy);
        }
        else {
          tmp_y  = &((double*)y)[ly];
        }
        
        if(type_tval  == NCL_double) tmp_tval  = &((double*)tval)[ln];
        if(type_rcoef == NCL_double) tmp_rcoef = &((double*)rcoef)[ln];
        
        NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                   tmp_rcoef,tmp_tval,&nptxy[ln],
                                   &xave,&yave,&ier);
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
 */
  if(ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regcoef: %i array(s) contained all missing values",ier_count5);
  }
  if (ier_count6) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regcoef: %i array(s) contained less than 3 non-missing values",ier_count6);
  }
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
    return(NclReturnValue(rcoef,ndims_rcoef,dsizes_rcoef,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(rcoef,ndims_rcoef,dsizes_rcoef,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes regCoef_W( void )
{
/*
 * Input array variables
 */
  void *x, *y;
  double *tmp_x, *tmp_y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y, npts;
/*
 * Attribute variables
 */
  int att_id, dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Output array variables
 */
  void *tval, *rcoef;
  double *tmp_tval, *tmp_rcoef, xave, yave;
  int ndims_rcoef, *dsizes_rcoef;
  NclBasicDataTypes type_rcoef;
  int *nptxy;
/*
 * various
 */
  int i, j, k, ly, lx, ln, dimsizes_same;
  int total_size_leftmost_x, total_size_leftmost_y; 
  int total_size_x, total_size_y, total_size_rcoef;
  int ier = 0, ier_count5 = 0, ier_count6 = 0;
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
           2);
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: The rightmost dimension of x must be at least 2");
    return(NhlFATAL);
  }  
  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];
  total_size_x = total_size_leftmost_x * npts;

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];
  total_size_y = total_size_leftmost_y * npts;

/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_rcoef  = max(1,ndims_x-1);
    dsizes_rcoef = (int*)calloc(ndims_rcoef,sizeof(int));
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
    dsizes_rcoef = (int*)calloc(ndims_rcoef,sizeof(int));
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
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  if(missing_dx.doubleval != missing_dy.doubleval) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: The x and y missing values must be the same");
    return(NhlFATAL);
  }
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

    rcoef     = (float *)calloc(total_size_rcoef,sizeof(float));
    tval      = (float *)calloc(total_size_rcoef,sizeof(float));
    nptxy     = (int *)calloc(total_size_rcoef,sizeof(int));
    tmp_tval  = (double*)calloc(1,sizeof(double));
    tmp_rcoef = (double *)calloc(1,sizeof(double));

    if(tmp_rcoef == NULL || rcoef == NULL || nptxy == NULL ||
       tmp_tval  == NULL || tval  == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
  else {
    type_rcoef = NCL_double;

    rcoef = (double *)calloc(total_size_rcoef,sizeof(double));
    tval  = (double *)calloc(total_size_rcoef,sizeof(double));
    nptxy = (int *)calloc(total_size_rcoef,sizeof(int));

    if(rcoef == NULL || tval == NULL || nptxy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regCoef: Unable to allocate memory for output variables");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'regcoef' with the full argument list.
 */
  if(dimsizes_same) {
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,lx,type_x,npts,0,
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
        coerce_subset_input_double(y,tmp_y,lx,type_y,npts,0,
                                   &missing_y,&missing_dy);
      }
      else {
/*
 * Point tmp_y to appropriate locations in y.
 */
        tmp_y = &((double*)y)[lx];
      }

      if(type_rcoef == NCL_double) {
        tmp_tval  = &((double*)tval)[ln];
        tmp_rcoef = &((double*)rcoef)[ln];
      }

      NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                 tmp_rcoef,tmp_tval,&nptxy[ln],
                                 &xave,&yave,&ier);
      if (ier == 5) ier_count5++;
      if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
      if(type_rcoef != NCL_double) {
        ((float*)tval)[ln]  = (float)*tmp_tval;
        ((float*)rcoef)[ln] = (float)*tmp_rcoef;
      }

      lx += npts;
      ln ++;
    }
  }
  else {
    lx = ln = 0;
    for(i = 1; i <= total_size_leftmost_x; i++) {
      ly = 0;
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_x != NCL_double) {
/*
 * Coerce npts subsection of x (tmp_x) to double.
 */
          coerce_subset_input_double(x,tmp_x,lx,type_x,npts,0,
                                     &missing_x,&missing_dx);
        }
        else {
          tmp_x  = &((double*)x)[lx];
        }
        
        if(type_y != NCL_double) {
/*
 * Coerce npts subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,ly,type_y,npts,0,
                                     &missing_y,&missing_dy);
        }
        else {
          tmp_y  = &((double*)y)[ly];
        }
        
        if(type_rcoef == NCL_double) {
          tmp_tval  = &((double*)tval)[ln];
          tmp_rcoef = &((double*)rcoef)[ln];
        }
        
        NGCALLF(dregcoef,DREGCOEF)(tmp_x,tmp_y,&npts,&missing_dx.doubleval,
                                   tmp_rcoef,tmp_tval,&nptxy[ln],
                                   &xave,&yave,&ier);
        if (ier == 5) ier_count5++;
        if (ier == 6) ier_count6++;
/*
 * Coerce output to float if necessary.
 */
        if(type_rcoef != NCL_double) {
          ((float*)tval)[ln]  = (float)*tmp_tval;
          ((float*)rcoef)[ln] = (float)*tmp_rcoef;
        }
        
        ly += npts;
        ln ++;
      }
      lx += npts;
    }
  }
/*
 * Handle error messages.
 */
  if(ier_count5) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef: %i array(s) contained all missing values",ier_count5);
  }
  if (ier_count6) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"regCoef: %i array(s) contained less than 3 non-missing values",ier_count6);
  }
/*
 * free memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_y     != NCL_double) NclFree(tmp_y);
  if(type_rcoef != NCL_double) {
    NclFree(tmp_rcoef);
    NclFree(tmp_tval);
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
                      &missing_rx,
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
                      &missing_dx,
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy, missing_rx;
  NclBasicDataTypes type_x, type_y;
  int has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  double *rcoef, *tval, *xave, *yave;
  float *rrcoef, *rtval,*rxave,*ryave;
  int *nptxy, ier = 0;

/*
 * Attribute variables
 */
  int att_id, dsizes[1];
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
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           2,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
/*
 * The x and y arrays coming in must be one-dimensional and have
 * the same length.
 */
  if( ndims_x != 1 || ndims_y != 1 || dsizes_x[0] != dsizes_y[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The input arrays must be 1-dimensional and be of the same length");
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
 * Coerce x and y to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
  dx = coerce_input_double(x,type_x,dsizes_x[0],has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,dsizes_y[0],has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing x and y arrays to double precision");
    return(NhlFATAL);
  }
/*
 * The x and y missing values must be the same.
 */
  if(missing_dx.doubleval != missing_dy.doubleval) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and y missing values must be the same");
    return(NhlFATAL);
  }

/*
 * Allocate space for output variables.
 */
  rcoef = (double *)calloc(1,sizeof(double));
  tval  = (double *)calloc(1,sizeof(double));
  xave  = (double *)calloc(1,sizeof(double));
  yave  = (double *)calloc(1,sizeof(double));
  nptxy =   (int *)calloc(1,sizeof(int));
  if( rcoef == NULL || tval == NULL || xave == NULL || yave == NULL ||
      nptxy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for output values");
    return(NhlFATAL);
  }

/*
 * Call the f77 version of 'regline' with the full argument list.
 */
  NGCALLF(dregcoef,DREGCOEF)(&dx[0],&dy[0],&npts,&missing_dx.doubleval,
                             rcoef,tval,nptxy,xave,yave,&ier);
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
    rxave  = (float *)calloc(1,sizeof(float));
    ryave  = (float *)calloc(1,sizeof(float));
    if( rrcoef == NULL || rtval == NULL || rxave == NULL || ryave == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for coercing output values back to floating point");
      return(NhlFATAL);
    }
/*
 * Coerce double to float.
 */
    *rrcoef = (float)*rcoef;
    *rtval  = (float)*tval;
    *rxave  = (float)*xave;
    *ryave  = (float)*yave;
/*
 * Free up variables holding double precision values.
 */
    NclFree(rcoef);
    NclFree(tval);
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
                      &missing_rx,
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
                      &missing_dx,
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


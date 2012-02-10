#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcalcorc,DCALCORC)(double*,double*,double*,double*,int*,
                                       double*,double*,double*,int*);

extern void NGCALLF(dcalcovc,DCALCOVC)(double*,double*,double*,double*,int*,
                                       double*,double*,double*,int*);

extern void NGCALLF(collapsexy,COLLAPSEXY)(double*,double*,int*,double*,
                                     double*,double*,double*,int*);

NhlErrorTypes escorc_W( void )
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
/*
 * Output array variables
 */
  void *corc;
  double *tmp_corc = NULL;
  int ndims_corc;
  ng_size_t dsizes_corc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_corc;
/*
 * various
 */
  double *xx, *yy;
  int nxy, mxy;
  ng_size_t i, j, index_x, index_y, index_corc;
  int ier, ier_count;
  ng_size_t dimsizes_same;
  ng_size_t total_size_leftmost_x, total_size_x;
  ng_size_t total_size_leftmost_y, total_size_y;
  ng_size_t total_size_corc;
  double xave, xstd, xvar;
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
 * The last dimension of x and y both must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: The last dimension of x must be equal to the last dimension of y");
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
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*
 * Compute the total number of elements in our arrays.
 */
  nxy = dsizes_x[ndims_x-1];

  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];
  total_size_x = total_size_leftmost_x * nxy;

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];
  total_size_y = total_size_leftmost_y * nxy;
/*
 * Coerce missing values to double.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

/*
 * Allocate space for temporary x array.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxy,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for temporary y array.
 */
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(nxy,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for collapsing x and y arrays if necessary.
 */
  xx = (double*)calloc(nxy,sizeof(double));
  yy = (double*)calloc(nxy,sizeof(double));
  if( xx == NULL || yy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_corc = max(1,ndims_x-1);
    total_size_corc = total_size_leftmost_x;
    if(ndims_x == 1) {
      dsizes_corc[0] = 1;
    }
    else {
      for( i = 0; i < ndims_x-1; i++ ) {
        dsizes_corc[i] = dsizes_x[i];
      }
    }
  }
  else {
    total_size_corc = total_size_leftmost_x * total_size_leftmost_y;
    ndims_corc = max(1,ndims_x + ndims_y - 2);
    dsizes_corc[0] = 1;

    for( i = 0; i < ndims_x-1; i++ ) {
      dsizes_corc[i] = dsizes_x[i];
    }
    for( i = 0; i < ndims_y-1; i++ ) {
      dsizes_corc[ndims_x-1+i] = dsizes_y[i];
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_corc = NCL_float;

    corc     = (void*)calloc(total_size_corc,sizeof(float));
    tmp_corc = (double *)calloc(1,sizeof(double));

    if (corc == NULL || tmp_corc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate space for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_corc = NCL_double;

    corc = (void*)calloc(total_size_corc,sizeof(double));
    if (corc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escorc: Unable to allocate space for output array");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  ier_count = index_x = index_corc = 0;

  if(dimsizes_same) {
    for(i = 1; i <= total_size_leftmost_x; i++) {
      xvar = xave = xstd = missing_dx.doubleval;
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,nxy,0,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[index_x];
      }

      if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,nxy,0,
                                   &missing_y,&missing_dy);
      }
      else {
/*
 * Point tmp_y to appropriate locations in y.
 */
        tmp_y = &((double*)y)[index_x];
      }
      if(type_corc == NCL_double) {
        tmp_corc = &((double*)corc)[index_corc];
      }
/*
 * Collapse x and y (remove missing values)
 */
      NGCALLF(collapsexy,COLLAPSEXY)(tmp_x,tmp_y,&nxy,&missing_dx.doubleval,
                                     &missing_dy.doubleval,xx,yy,&mxy);

      if(mxy > 0) {
        xave = xstd = missing_dx.doubleval;
        NGCALLF(dcalcorc,DCALCORC)(xx,&xave,&xstd,&missing_dx.doubleval,
                                   &mxy,yy,&missing_dy.doubleval,
                                   tmp_corc,&ier);
/*
 * Copy output values from temporary array "tmp_corc" to final array "corc".
 */
        if(type_corc != NCL_double) {
          ((float*)corc)[index_corc] = (float)(*tmp_corc);
        } 

        if(ier != 0) ier_count++;
      }
      else {
/*
 * Fill this subsection of the output with missing values.
 */
        if(type_corc != NCL_double) {
          ((float*)corc)[index_corc] = missing_rx.floatval;
        } 
        else {
          ((double*)corc)[index_corc] = missing_dx.doubleval;
        }
      }
      index_corc++;
      index_x   += nxy;
    }
  }
  else {
    for(i = 1; i <= total_size_leftmost_x; i++) {
      index_y = 0;
      xvar = xave = xstd = missing_dx.doubleval;
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,nxy,0,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[index_x];
      }
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,nxy,0,
                                     &missing_y,&missing_dy);
        }
        else {
/*
 * Point tmp_y to appropriate locations in y.
 */
          tmp_y = &((double*)y)[index_y];
        }
        if(type_corc == NCL_double) {
          tmp_corc = &((double*)corc)[index_corc];
        }

/*
 * Collapse x and y (remove missing values)
 */
        NGCALLF(collapsexy,COLLAPSEXY)(tmp_x,tmp_y,&nxy,&missing_dx.doubleval,
                                       &missing_dy.doubleval,xx,yy,&mxy);

        xave = xstd = missing_dx.doubleval;
        NGCALLF(dcalcorc,DCALCORC)(xx,&xave,&xstd,&missing_dx.doubleval,
                                   &mxy,yy,&missing_dy.doubleval,
                                   tmp_corc,&ier);
        if(mxy > 0) {
/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
          if(type_corc != NCL_double) {
            ((float*)corc)[index_corc] = (float)(*tmp_corc);
          } 

          if(ier != 0) ier_count++;
        }
        else {
/*
 * Fill this subsection of the output with missing values.
 */
          if(type_corc != NCL_double) {
            ((float*)corc)[index_corc] = missing_rx.floatval;
          } 
          else {
            ((double*)corc)[index_corc] = missing_dx.doubleval;
          }
        }
        index_corc++;
        index_y += nxy;
      }
      index_x += nxy;
    }
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"escorc: Non-fatal conditions encountered in series or xstd equals zero.\nPossibly, all values of a series are constant.");
    NhlPError(NhlWARNING,NhlEUNKNOWN,"escorc: Most likely, one or more series consisted of all constant values");
  }


/*
 * free memory.
 */
  NclFree(xx);
  NclFree(yy);
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_corc != NCL_double) NclFree(tmp_corc);

/*
 * Return values. 
 */
  if(type_corc != NCL_double) {
/*
 * Neither input array is double, so return float values.
 */
    return(NclReturnValue(corc,ndims_corc,dsizes_corc,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * At least one input array was double, so return double values.
 */
    return(NclReturnValue(corc,ndims_corc,dsizes_corc,&missing_dx,
                          NCL_double,0));
  }
}

NhlErrorTypes escovc_W( void )
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
/*
 * Output array variables
 */
  void *covc;
  double *tmp_covc = NULL;
  int ndims_covc;
  ng_size_t dsizes_covc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_covc;
/*
 * various
 */
  double *xx, *yy;
  ng_size_t index_x, index_y, index_covc;
  int ier, ier_count;
  int nxy, mxy;
  ng_size_t i, j;
  ng_size_t dimsizes_same;
  ng_size_t total_size_leftmost_x, total_size_x;
  ng_size_t total_size_leftmost_y, total_size_y;
  ng_size_t total_size_covc;
  double xave, xstd, xvar;
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
 * The last dimension of x and y both must be the same.
 */
  if( dsizes_x[ndims_x-1] != dsizes_y[ndims_y-1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: The last dimension of x must be equal to the last dimension of y");
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
      }
    }
  }
  else {
    dimsizes_same = 0;
  }
/*
 * Compute the total number of elements in our arrays.
 */
  nxy = dsizes_x[ndims_x-1];

  total_size_leftmost_x = 1;
  for(i = 0; i < ndims_x-1; i++) total_size_leftmost_x *= dsizes_x[i];
  total_size_x = total_size_leftmost_x * nxy;

  total_size_leftmost_y = 1;
  for(i = 0; i < ndims_y-1; i++) total_size_leftmost_y *= dsizes_y[i];
  total_size_y = total_size_leftmost_y * nxy;
/*
 * Coerce missing values to double.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);

/*
 * Allocate space for temporary x array.
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(nxy,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for temporary y array.
 */
  if(type_y != NCL_double) {
    tmp_y = (double*)calloc(nxy,sizeof(double));
    if( tmp_y == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for collapsing x and y arrays if necessary.
 */
  xx = (double*)calloc(nxy,sizeof(double));
  yy = (double*)calloc(nxy,sizeof(double));
  if( xx == NULL || yy == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/* 
 * Get size of output variable, which is equal to the product of all but
 * the last dimension of x and y (unless the dimension sizes of x and y
 * are the same, in which case the output will be the product of the all
 * but the last dimension of x).
 */
  if(dimsizes_same) {
    ndims_covc = max(1,ndims_x-1);
    total_size_covc = total_size_leftmost_x;
    if(ndims_x == 1) {
      dsizes_covc[0] = 1;
    }
    else {
      for( i = 0; i < ndims_x-1; i++ ) {
        dsizes_covc[i] = dsizes_x[i];
      }
    }
  }
  else {
    total_size_covc = total_size_leftmost_x * total_size_leftmost_y;
    ndims_covc = max(1,ndims_x + ndims_y - 2);
    dsizes_covc[0] = 1;

    for( i = 0; i < ndims_x-1; i++ ) {
      dsizes_covc[i] = dsizes_x[i];
    }
    for( i = 0; i < ndims_y-1; i++ ) {
      dsizes_covc[ndims_x-1+i] = dsizes_y[i];
    }
  }
  if(type_x != NCL_double && type_y != NCL_double) {
    type_covc = NCL_float;

    covc     = (void*)calloc(total_size_covc,sizeof(float));
    tmp_covc = (double *)calloc(1,sizeof(double));

    if (covc == NULL || tmp_covc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: Unable to allocate space for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_covc = NCL_double;

    covc = (void*)calloc(total_size_covc,sizeof(double));
    if (covc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"escovc: Unable to allocate space for output array");
      return(NhlFATAL);
    }
  }
/*
 * Call the f77 version of 'descros' with the full argument list.
 */
  ier_count = index_x = index_covc = 0;

  if(dimsizes_same) {
    for(i = 1; i <= total_size_leftmost_x; i++) {
      xvar = xave = xstd = missing_dx.doubleval;
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,nxy,0,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[index_x];
      }

      if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
        coerce_subset_input_double(y,tmp_y,index_x,type_y,nxy,0,
                                   &missing_y,&missing_dy);
      }
      else {
/*
 * Point tmp_y to appropriate locations in y.
 */
        tmp_y = &((double*)y)[index_x];
      }
      if(type_covc == NCL_double) {
        tmp_covc = &((double*)covc)[index_covc];
      }
/*
 * Collapse x and y (remove missing values)
 */
      NGCALLF(collapsexy,COLLAPSEXY)(tmp_x,tmp_y,&nxy,&missing_dx.doubleval,
                                     &missing_dy.doubleval,xx,yy,&mxy);

      if(mxy > 0) {
        xave = xstd = missing_dx.doubleval;
        NGCALLF(dcalcovc,DCALCOVC)(xx,&xave,&xstd,&missing_dx.doubleval,
                                   &mxy,yy,&missing_dy.doubleval,
                                   tmp_covc,&ier);
/*
 * Copy output values from temporary array "tmp_covc" to final array "covc".
 */
        if(type_covc != NCL_double) {
          ((float*)covc)[index_covc] = (float)(*tmp_covc);
        } 

        if(ier != 0) ier_count++;
      }
      else {
/*
 * Fill this subsection of the output with missing values.
 */
        if(type_covc != NCL_double) {
          ((float*)covc)[index_covc] = missing_rx.floatval;
        } 
        else {
          ((double*)covc)[index_covc] = missing_dx.doubleval;
        }
      }
      index_covc++;
      index_x   += nxy;
    }
  }
  else {
    for(i = 1; i <= total_size_leftmost_x; i++) {
      index_y = 0;
      xvar = xave = xstd = missing_dx.doubleval;
      if(type_x != NCL_double) {
/*
 * Coerce nxy subsection of x (tmp_x) to double.
 */
        coerce_subset_input_double(x,tmp_x,index_x,type_x,nxy,0,
                                   &missing_x,&missing_dx);
      }
      else {
/*
 * Point tmp_x to appropriate locations in x.
 */
        tmp_x = &((double*)x)[index_x];
      }
      for(j = 1; j <= total_size_leftmost_y; j++) {
        if(type_y != NCL_double) {
/*
 * Coerce nxy subsection of y (tmp_y) to double.
 */
          coerce_subset_input_double(y,tmp_y,index_y,type_y,nxy,0,
                                     &missing_y,&missing_dy);
        }
        else {
/*
 * Point tmp_y to appropriate locations in y.
 */
          tmp_y = &((double*)y)[index_y];
        }
        if(type_covc == NCL_double) {
          tmp_covc = &((double*)covc)[index_covc];
        }

/*
 * Collapse x and y (remove missing values)
 */
        NGCALLF(collapsexy,COLLAPSEXY)(tmp_x,tmp_y,&nxy,&missing_dx.doubleval,
                                       &missing_dy.doubleval,xx,yy,&mxy);

        xave = xstd = missing_dx.doubleval;
        NGCALLF(dcalcovc,DCALCOVC)(xx,&xave,&xstd,&missing_dx.doubleval,
                                   &mxy,yy,&missing_dy.doubleval,
                                   tmp_covc,&ier);
        if(mxy > 0) {
/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
          if(type_covc != NCL_double) {
            ((float*)covc)[index_covc] = (float)(*tmp_covc);
          } 

          if(ier != 0) ier_count++;
        }
        else {
/*
 * Fill this subsection of the output with missing values.
 */
          if(type_covc != NCL_double) {
            ((float*)covc)[index_covc] = missing_rx.floatval;
          } 
          else {
            ((double*)covc)[index_covc] = missing_dx.doubleval;
          }
        }
        index_covc++;
        index_y += nxy;
      }
      index_x += nxy;
    }
  }
  if(ier_count > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"escovc: Non-fatal conditions encountered in series or xstd = 0.0");
  }

/*
 * free memory.
 */
  NclFree(xx);
  NclFree(yy);
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_y    != NCL_double) NclFree(tmp_y);
  if(type_covc != NCL_double) NclFree(tmp_covc);

/*
 * Return values. 
 */
  if(type_covc != NCL_double) {
/*
 * Neither input array is double, so return float values.
 */
    return(NclReturnValue(covc,ndims_covc,dsizes_covc,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * At least one input array was double, so return double values.
 */
    return(NclReturnValue(covc,ndims_covc,dsizes_covc,&missing_dx,
                          NCL_double,0));
  }
}

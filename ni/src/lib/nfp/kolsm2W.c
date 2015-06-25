#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"

extern void NGCALLF(kolm2,KOLM2)(double *, double *, int *, int *,
                                 int *, double *, double *, double *);

NhlErrorTypes kolsm2_n_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  int *dims;
  ng_size_t dsizes_dims[1], ndims;

/*
 * Return variable
 */
  void *prob;
  double tmp_prob;
  int ndims_prob;
  ng_size_t *dsizes_prob, nprob;
  NclBasicDataTypes type_prob;
  NclObjClass type_prob_class;

/*
 * Various
 */
  ng_size_t i, j, nx, ny, np;
  ng_size_t size_leftmost, size_rightmost;
  ng_size_t index_nrx, index_nry, index_x, index_y, nrnx, nrny;
  int inx, iny, iflag=1;

/*
 * Attribute variables
 */
  void *zstat, *dstat;
  double tmp_zstat, tmp_dstat;
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           3,
           &ndims_y,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           DONT_CARE);

  if(ndims_x != ndims_y) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: x and y must be the same rank");
    return(NhlFATAL);
  }
/*
 * Get argument # 2. This will be a 1D array of dimension indexes.
 */
  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Make sure "dims" are valid dimensions.
 */
  ndims = dsizes_dims[0];
  if(ndims > ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: too many input dimensions were given.");
    return(NhlFATAL);
  }
/* 
 * Dimensions must be increasing and consecutive, or else just equal to 
 * the scalar -1.
 */
  if(!(ndims == 1 && dims[0] == -1)) {
    for(i = 0; i < ndims; i++) {
      if (dims[i] < 0 || dims[i] >= ndims_x) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: One or more input dimension indexes are invalid");
        return(NhlFATAL);
      }
      if((i > 0) && (dims[i]-dims[i-1] != 1)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: input dimension sizes must be consecutive and increasing");
        return(NhlFATAL);
      }
    }
  }
/*
 * Collect the dimensions that are *not* in dims. These dimensions must 
 * match in X and Y. These dimensions will also determine the size
 * and dimensionality of the output array.
 *
 * dims = -1 is a special case that says treat x,y as one giant array, 
 * and hence just return a scalar.
 */
  if((ndims == ndims_x) || (ndims == 1 && dims[0] == -1)) {
    nprob          = 1;
    ndims_prob     = 1;
    dsizes_prob    = (ng_size_t*)calloc(1,sizeof(ng_size_t)); 
    dsizes_prob[0] = 1;
    size_rightmost = size_leftmost = 1;
    nx = ny = 1;
    for(i=0; i < ndims_x; i++) {
      nx *= dsizes_x[i];
      ny *= dsizes_y[i];
    }
  }    
  else {
    ndims_prob  = ndims_x - ndims;
    dsizes_prob = (ng_size_t*)calloc(ndims_prob,sizeof(ng_size_t));
/*
 * Calculate the size of the leftmost dimensions of x/y and the
 * number of input points for x and y. At the same time, check that
 * the dimension sizes of x and y in these locations are the same.
 */
    size_leftmost = 1;
    np = 0;
    for( i = 0; i < dims[0]; i++ ) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: x and y must be the same size in the dimensions *not* indicated by 'dims'");
        return(NhlFATAL);
      }
      size_leftmost *= dsizes_x[i];
      dsizes_prob[np] = dsizes_x[i];
      np++;
    }
/*
 * Calculate the size of nx,ny
 */
    nx = ny = 1;
    for( i = 0; i < ndims; i++) {
      nx *= dsizes_x[dims[i]];
      ny *= dsizes_y[dims[i]];
    }
/*
 * Calculate the size of the rightmost dimensions of x/y and the
 * remaining number of input points for x and y. Again, also check that
 * the dimension sizes of x and y in these locations are the same.
 */
    size_rightmost = 1;
    for( i = dims[ndims-1]+1; i < ndims_x; i++ ) {
      if(dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: x and y must be the same size in the dimensions *not* indicated by 'dims'");
        return(NhlFATAL);
      }
      size_rightmost *= dsizes_x[i];
      dsizes_prob[np] = dsizes_x[i];
      np++;
    }
  }
  nprob = size_leftmost * size_rightmost;
  if((nx > INT_MAX) || (ny > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int)nx;
  iny = (int)ny;

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_x and tmp_y.
 */
  tmp_x = (double *)calloc(nx,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: Unable to allocate memory for coercing x to double");
    return(NhlFATAL);
  }
  tmp_y = (double *)calloc(ny,sizeof(double));
  if(tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: Unable to allocate memory for coercing y array to double");
    return(NhlFATAL);
  }
  if(type_x == NCL_double || type_y == NCL_double) {
    type_prob       = NCL_double;
    type_prob_class = nclTypedoubleClass;
  }
  else {
    type_prob       = NCL_float;
    type_prob_class = nclTypefloatClass;
  }

/* 
 * Allocate space for output array and attributes.
 */
  if(type_prob != NCL_double) {
    prob  = (void *)calloc(nprob, sizeof(float));
    zstat = (void *)calloc(nprob, sizeof(float));
    dstat = (void *)calloc(nprob, sizeof(float));
  }
  else {
    prob  = (void *)calloc(nprob, sizeof(double));
    zstat = (void *)calloc(nprob, sizeof(double));
    dstat = (void *)calloc(nprob, sizeof(double));
  }
  if(prob == NULL || zstat == NULL || dstat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kolsm2_n: Unable to allocate memory for output array and attributes");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  nrnx = nx * size_rightmost;
  nrny = ny * size_rightmost;

  np = 0;
  for(i = 0; i < size_leftmost; i++) {
    index_nrx = i*nrnx;
    index_nry = i*nrny;
    for( j = 0; j < size_rightmost; j++ ) {
      index_x = index_nrx + j;
      index_y = index_nry + j;
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,
                                      size_rightmost,type_x,
                                      nx,0,NULL,NULL);
      coerce_subset_input_double_step(y,tmp_y,index_y,
                                      size_rightmost,type_y,
                                      ny,0,NULL,NULL);
/* 
 * Always sort x and y no matter what.
 */
      qsort((void*)tmp_x,nx,sizeof(double),cmpdouble);
      qsort((void*)tmp_y,ny,sizeof(double),cmpdouble);
/*
 * Call the Fortran routine.
 */
      
      NGCALLF(kolm2,KOLM2)(tmp_x, tmp_y, &inx, &iny, &iflag, 
                           &tmp_zstat, &tmp_prob, &tmp_dstat);
/*
 * Coerce output back to float if necessary.
 */
      coerce_output_float_or_double(prob, &tmp_prob, type_prob,1,np);
      coerce_output_float_or_double(zstat,&tmp_zstat,type_prob,1,np);
      coerce_output_float_or_double(dstat,&tmp_dstat,type_prob,1,np);
      np++; 
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);

/*
 * Set up return structure.
 */
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            prob,
                            NULL,
                            ndims_prob,
                            dsizes_prob,
                            TEMPORARY,
                            NULL,
                            type_prob_class
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
                         zstat,
                         NULL,
                         ndims_prob,
                         dsizes_prob,
                         TEMPORARY,
                         NULL,
                         type_prob_class
                         );

  _NclAddAtt(
             att_id,
             "zstat",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         dstat,
                         NULL,
                         ndims_prob,
                         dsizes_prob,
                         TEMPORARY,
                         NULL,
                         type_prob_class
                         );

  _NclAddAtt(
             att_id,
             "dstat",
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

  NclFree(dsizes_prob);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);

  return(NhlNOERROR);
}


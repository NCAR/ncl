#include <stdio.h>
#include "wrapper.h"

NhlErrorTypes sparse_matrix_mult_W
#if NhlNeedProto
(void)
#else
()
#endif
{
    /* Locally used variables */
    ng_size_t nvector, nmatrices, ntotal;
    ng_size_t ncoly, nrowy, nrowcoly, ncolx, nrowx, nrowcolx;
    ng_size_t i, j, k, l, index_x, index_y;
    ng_size_t xInd, yInd;
    int ndims;
    logical found_missing;

    /* Defining the arguments */
    /* Argument # 0 */
    void *row_in;
    ng_size_t *row;
    ng_size_t dsizes_row[1];
    NclBasicDataTypes type_row;
    
    /* Argument # 1 */
    void *col_in;
    ng_size_t *col;
    ng_size_t dsizes_col[1];
    NclBasicDataTypes type_col;
    
    /* Argument # 2 */
    void   *S;
    double *dS;
    NclBasicDataTypes type_S;
    ng_size_t dsizes_S[1];
    
    /* Argument # 3 */
    void   *x;
    double *dx;
    NclScalar missing_x, missing_dx, missing_fx;
    int  ndims_x, has_missing_x;
    NclBasicDataTypes type_x;
    ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
    
    /* Argument # 4 */
    void *tmp_output_dsizes;
    ng_size_t *output_dsizes;
    ng_size_t dsizes_output_dsizes[1];
    NclBasicDataTypes type_output_dsizes;

    /* Output argument */
    void   *y;
    double *dy;
    NclBasicDataTypes type_y;
    ng_size_t *dsizes_y;
    int ret;

    /* Getting Arguments values */

    /* Argument # 0 */
    row_in = (void*)NclGetArgValue(
                0,
                5,
                NULL,
                dsizes_row,
                NULL,
                NULL,
                &type_row,
                DONT_CARE);    

    /* Argument # 1 */
    col_in = (void*)NclGetArgValue(
                1,
                5,
                NULL,
                dsizes_col,
                NULL,
                NULL,
                &type_col,
                DONT_CARE);    


    /* Argument # 2 */
    S = (void*)NclGetArgValue(
                2,
                5,
                NULL,
                dsizes_S,
                NULL,
                NULL,
                &type_S,
                DONT_CARE); 

    if(dsizes_col[0] != dsizes_S[0] || dsizes_col[0] != dsizes_row[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: row, col, and S must have same number of elements");
        return(NhlFATAL);
    }

  /* Convert the row,col indexes to ng_size_t. */
    ndims = (int)dsizes_row[0];
    row = get_dimensions(row_in,ndims,type_row,"sparse_matrix_mult");
    col = get_dimensions(col_in,ndims,type_col,"sparse_matrix_mult");
    if(row == NULL || col == NULL) 
      return(NhlFATAL);

      /* Argument # 3 */
    x = (void*)NclGetArgValue(
                3,
                5,
                &ndims_x,
                dsizes_x,
                &missing_x,
                &has_missing_x,
                &type_x,
                DONT_CARE); 

    if(ndims_x < 2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: x must be at least a 2-dimensional array");
      return(NhlFATAL);
    }

    nrowx = dsizes_x[ndims_x-2];
    ncolx = dsizes_x[ndims_x-1];

      /* Argument #4 */
    tmp_output_dsizes = (void*)NclGetArgValue(
           4,
           5,
           NULL,
           dsizes_output_dsizes,
           NULL,
           NULL,
           &type_output_dsizes,
           DONT_CARE);

/* Convert the output dimensions to ng_size_t. */
    output_dsizes = get_dimensions(tmp_output_dsizes,dsizes_output_dsizes[0],
                                   type_output_dsizes,"sparse_matrix_mult");
    if(output_dsizes == NULL) 
      return(NhlFATAL);

    /* nrowy and ncoly for output array */
    nrowy = output_dsizes[0];
    ncoly = output_dsizes[1];

    if(ncolx != ncoly) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: input array is not the correct dimensionality");
      return(NhlFATAL);
    }
    nvector   = dsizes_S[0];
    nrowcoly  = nrowy * ncoly;
    nrowcolx  = nrowx * ncolx;

    dsizes_y = (ng_size_t*)malloc(ndims_x*sizeof(ng_size_t));
    nmatrices = 1;
    for(i = 0; i < ndims_x-2; i++) {
      nmatrices *= dsizes_x[i];
      dsizes_y[i] = dsizes_x[i];
    }
    dsizes_y[ndims_x-2] = nrowy;
    dsizes_y[ndims_x-1] = ncoly;
    ntotal = nmatrices * nrowcoly;

/*
 * Coerce missing values to double if necessary.
 */
    coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_fx);

/*
 * Coerce S to double if necessary.
 */
    dS = coerce_input_double(S,type_S,dsizes_S[0],0,NULL,NULL);
    if(dS == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for coercing input to double");
      return(NhlFATAL);
    }    
/*
 * Create temporary array for x no matter what, because we might need
 * to set values to missing.
 */
    dx = (double*)calloc(nrowcolx,sizeof(double));
    if(dx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for coercing input to double");
      return(NhlFATAL);
    }
/*
 * Create output variable.
 */
    if(type_x == NCL_double || type_S == NCL_double) {
      type_y = NCL_double;
      y      = (void*)calloc(ntotal,sizeof(double));
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
      type_y = NCL_float;
      y      = (void*)calloc(ntotal,sizeof(float));
      dy     = (double*)calloc(nrowcoly,sizeof(double));
      if(y == NULL || dy == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    
/*
 * Loop across each set of 2D matrices and do the sparse
 * matrix multiplication.
 */
    for(l = 0; l < nmatrices; l++) {
      index_x = l*nrowcolx;
      index_y = l*nrowcoly;
/*
 * Coerce nrowx x ncolx subsection of x (dx) to double.
 */
      coerce_subset_input_double(x,dx,index_x,type_x,nrowcolx,0,NULL,NULL);

/*
 * Point dy to appropriate location in y
 */
      if(type_y == NCL_double) {
        dy = &((double*)y)[index_y];
      }
      else if(l) {
/*
 * Be sure to zero out the dy array again (if it's not
 * pointing into the "y" array which is already zeroed out).
 */
        for (i = 0; i < nrowcoly; i++) dy[i] = 0.0;
      }

      if (has_missing_x) {
/* 
 * Search ahead for missing values. If there's a missing value
 * in any column of "x", then all values in that column of "y" will
 * also be missing. Go ahead and set "x" to missing too, for later.
 */
        for (i = 0; i < ncolx; i++) {
          found_missing = False;
          j = 0;
          while( j < nrowx && !found_missing ) {
            xInd = j*ncolx+i;
            if (dx[xInd] == missing_dx.doubleval) {
              found_missing = True;
              for (k = 0; k < nrowy; k++) {
                yInd = k*ncoly+i;
                dy[yInd] = missing_dx.doubleval;
              }
              for (k = 0; k < nrowx; k++) {
                xInd = k*ncolx+i;
                dx[xInd] = missing_dx.doubleval;
              }
            }
            j++;
          }
        }

/*        
 * This is the loop for the calculation.
 */
        for(i = 0; i < nvector; i++) {
          for (j = 0; j < ncoly; j++) {
            xInd = col[i]*ncoly+j;
            yInd = row[i]*ncoly+j;
            if (dx[xInd] != missing_dx.doubleval) {
              dy[yInd] += dx[xInd]*dS[i];
            }
          }
        }
      }
      else {
/*
 * Case for no input missing values.
 */
        for(i = 0; i < nvector; i++) {
          for (j = 0; j < ncoly; j++) {
            xInd = col[i]*ncoly+j;
            yInd = row[i]*ncoly+j;
            dy[yInd] += dx[xInd]*dS[i];
          }
        }
      }
/*
 * Coerce output back to float if necessary.
 */
      if(type_y == NCL_float) {
        coerce_output_float_only(y,dy,nrowcoly,index_y);
      }
    }
/*
 * Clean up
 */
    NclFree(dx);
    if(type_S != NCL_double) NclFree(dS);
    if(type_y != NCL_double) NclFree(dy);
    NclFree(row);
    NclFree(col);

/*
 * Return value back to NCL script.
 */
    if(has_missing_x) {
      if(type_y == NCL_double) {
        ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_dx,type_y,0);
      }
      else {
        ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_fx,type_y,0);
      }
    }
    else {
      ret = NclReturnValue(y,ndims_x,dsizes_y,NULL,type_y,0);
    }
    NclFree(dsizes_y);
    return(ret);
}

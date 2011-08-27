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
    ng_size_t ncol, nrow, nrowcol;
    ng_size_t i, j, k, l, index_x;
    ng_size_t xInd, yInd;
    int ndims;
    
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
    
    /* Output argument # 4 */
    void   *y;
    double *dy;
    NclBasicDataTypes type_y;
    int ret;

    /* Getting Arguments values */

    /* Argument # 0 */
    row_in = (void*)NclGetArgValue(
                0,
                4,
                NULL,
                dsizes_row,
                NULL,
                NULL,
                &type_row,
                DONT_CARE);    

    /* Argument # 1 */
    col_in = (void*)NclGetArgValue(
                1,
                4,
                NULL,
                dsizes_col,
                NULL,
                NULL,
                &type_col,
                DONT_CARE);    


    if(dsizes_row[0] != dsizes_col[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: row and col must have same number of elements");
        return(NhlFATAL);
    }

/*
 * Convert the input dimensions to ng_size_t.
 */
    ndims = (int)dsizes_row[0];
    row = get_dimensions(row_in,ndims,type_row,"sparse_matrix_mult");
    col = get_dimensions(col_in,ndims,type_col,"sparse_matrix_mult");
    if(row == NULL || col == NULL) 
      return(NhlFATAL);

    /* Argument # 2 */
    S = (void*)NclGetArgValue(
                2,
                4,
                NULL,
                dsizes_S,
                NULL,
                NULL,
                &type_S,
                DONT_CARE); 

    if(dsizes_col[0] != dsizes_S[0]) { 
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: row, col, and S must have same number of elements");
        return(NhlFATAL);
    }

      /* Argument # 3 */
    x = (void*)NclGetArgValue(
                3,
                4,
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
    nvector   = dsizes_S[0];
    nrow      = dsizes_x[ndims_x-2];
    ncol      = dsizes_x[ndims_x-1];
    nrowcol   = nrow * ncol;
    nmatrices = 1;
    for(i = 0; i < ndims_x-2; i++) {
      nmatrices *= dsizes_x[i];
    }
    ntotal = nmatrices * nrowcol;

/* Error checking for input vector indexes. */
    for(i = 0; i < nvector; i++) {
      if(row[i] < 0 || row[i] >= nrow) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: invalid row index");
        return(NhlFATAL);
      }
      if(col[i] < 0 || col[i] >= ncol) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: invalid column index");
        return(NhlFATAL);
      }
    }
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
 * Create temporary array for x if necessary.
 */
    if(type_x != NCL_double) {
      dx = (double*)calloc(nrowcol,sizeof(double));
      if(dx == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for coercing input to double");
        return(NhlFATAL);
      }
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
      dy     = (double*)calloc(nrowcol,sizeof(double));
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
      index_x = l*nrowcol;
/*
 * Coerce nrow x ncol subsection of x (dx) to double.
 */
      if(type_x != NCL_double) {
        coerce_subset_input_double(x,dx,index_x,type_x,nrowcol,0,NULL,NULL);
      }
      else {
        dx = &((double*)x)[index_x];
      }
/*
 * Point dy to appropriate location in y
 */
      if(type_y == NCL_double) dy = &((double*)y)[index_x];

      if (has_missing_x) {
/* 
 * Search ahead for missing values. If there's a missing value
 * in any column of "x", then all values in that column of "y" will
 * also be missing. Go ahead and set "x" to missing too, for later.
 */
        for (i = 0; i < nrow; i++) {
          for (j = 0; j < ncol; j++) {
            xInd = i*nrow+j;
            if (dx[xInd] == missing_dx.doubleval) {
              for (k = 0; k < nrow; k++) {
                xInd = k*nrow+j;
                dx[xInd] = missing_dx.doubleval;
                dy[xInd] = missing_dx.doubleval;
              }
            }
            else{
              dy[xInd] = 0.0;
            }
          }
        }
/*        
 * This is the loop for the calculation.
 */
        for(i = 0; i < nvector; i++) {
          for (j = 0; j < ncol; j++) {
            xInd = col[i]*ncol+j;
            yInd = row[i]*ncol+j;
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
        for (i = 0; i < nrow; i++) {
          for (j = 0; j < ncol; j++) {
            xInd = i*nrow+j;
            dy[xInd] = 0.0;
          }
        }
        for(i = 0; i < nvector; i++) {
          for (j = 0; j < ncol; j++) {
            xInd = col[i]*ncol+j;
            yInd = row[i]*ncol+j;
            dy[yInd] += dx[xInd]*dS[i];
          }
        }
      }
/*
 * Coerce output back to float if necessary.
 */
      if(type_y == NCL_float) {
        coerce_output_float_only(y,dy,nrowcol,index_x);
      }
    }
/*
 * Clean up
 */
    if(type_x != NCL_double) NclFree(dx);
    if(type_S != NCL_double) NclFree(dS);
    if(type_y != NCL_double) NclFree(dy);
    NclFree(row);
    NclFree(col);

/*
 * Return value back to NCL script.
 */
    if(has_missing_x) {
      if(type_y == NCL_double) {
        ret = NclReturnValue(y,ndims_x,dsizes_x,&missing_dx,type_y,0);
      }
      else {
        ret = NclReturnValue(y,ndims_x,dsizes_x,&missing_fx,type_y,0);
      }
    }
    else {
      ret = NclReturnValue(y,ndims_x,dsizes_x,NULL,type_y,0);
    }
    return(ret);
}

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
    ng_size_t i, j, l, index_x, index_y;
    ng_size_t xInd, yInd;
    ng_size_t *iy;
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

    if(dsizes_output_dsizes[0] < 0 || dsizes_output_dsizes[0] > 2)  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: the output dimensions must represent a 1D or 2D array.");
      return(NhlFATAL);
    }


/* Convert the output dimensions to ng_size_t. */
    output_dsizes = get_dimensions(tmp_output_dsizes,dsizes_output_dsizes[0],
                                   type_output_dsizes,"sparse_matrix_mult");

/* 
 * Get nrowy and ncoly for output array.
 *
 * This function handles a special case where the dimension 
 * sizes for the output array can represent a 1D array rather
 * than a 2D matrix. If it's a 1D array, then it will be 
 * treated as an [nrow,1] array.
 * 
 * If the output dims are for a 2D array, then x must be at
 * least a 2D array.
 */
    if(dsizes_output_dsizes[0] == 2) {
      if(ndims_x < 2) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: the input array must be at least 2-dimensional if the output dimensions represent a 2D array");
        return(NhlFATAL);
      }
      nrowx = dsizes_x[ndims_x-2];
      ncolx = dsizes_x[ndims_x-1];
      nrowy = output_dsizes[0];
      ncoly = output_dsizes[1];
    }
    else {
      nrowx = dsizes_x[ndims_x-1];
      ncolx = 1;
      nrowy = output_dsizes[0];
      ncoly = 1;
    }

#ifdef DEBUG
    printf("nrowx = %ld\n", nrowx);
    printf("ncolx = %ld\n", ncolx);
    printf("nrowy = %ld\n", nrowy);
    printf("ncoly = %ld\n", ncoly);
#endif

    if(ncolx != ncoly) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: input array is not the correct dimensionality");
      return(NhlFATAL);
    }
    nvector   = dsizes_S[0];
    nrowcoly  = nrowy * ncoly;
    nrowcolx  = nrowx * ncolx;

    dsizes_y = (ng_size_t*)malloc(ndims_x*sizeof(ng_size_t));
    nmatrices = 1;
    if(dsizes_output_dsizes[0] == 2) {
      for(i = 0; i < ndims_x-2; i++) {
        nmatrices *= dsizes_x[i];
        dsizes_y[i] = dsizes_x[i];
      }
      dsizes_y[ndims_x-2] = nrowy;
      dsizes_y[ndims_x-1] = ncoly;
    }
    else {
      for(i = 0; i < ndims_x-1; i++) {
        nmatrices *= dsizes_x[i];
        dsizes_y[i] = dsizes_x[i];
      }
      dsizes_y[ndims_x-1] = nrowy;
    }
    ntotal = nmatrices * nrowcoly;
#ifdef DEBUG
    printf("nmatrices = %ld\n", nmatrices);
    for(i = 0; i < ndims_x; i++) {
      printf("dsizes_y[%ld] =  %ld\n", i, dsizes_y[i]);
    }
#endif

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
 * Create output variable. Create space for dy no matter what, because we need
 * to set it to zero, but y to missing.
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
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    dy = (double*)calloc(nrowcoly,sizeof(double));
    iy = (ng_size_t*)calloc(nrowcoly,sizeof(ng_size_t));
    if(iy == NULL || dy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }

/*
 * Set all output Y values to missing initially.
 */
    if(type_y == NCL_double) {
      for(i = 0; i < ntotal; i++) ((double*)y)[i] = missing_dx.doubleval;
    }
    else {
      for(i = 0; i < ntotal; i++) ((float*)y)[i] = missing_fx.floatval;
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

/* These arrays need to be zeroed out every time. */
      for (i = 0; i < nrowcoly; i++) {
        dy[i] = 0.0;
        iy[i] = 0;
      }

      if (has_missing_x) {
/*        
 * This is the loop for the calculation, with checks for missing x values.
 */
        for(i = 0; i < nvector; i++) {
          for (j = 0; j < ncoly; j++) {
            xInd = col[i]*ncoly+j;
            yInd = row[i]*ncoly+j;
            if (dx[xInd] != missing_dx.doubleval) {
              dy[yInd] += dx[xInd]*dS[i];
              iy[yInd] = 1;   /* keep track of where there are values */
            }
          }
        }
      }
      else {
/*
 * This is the loop for the calculation, with NO checks for missing x values.
 */
        for(i = 0; i < nvector; i++) {
#ifdef DEBUG
	  printf("dS[%ld] = %g, col = %ld row = %ld\n",i, dS[i], col[i], row[i]);
#endif
          for (j = 0; j < ncoly; j++) {
            xInd = col[i]*ncoly+j;
            yInd = row[i]*ncoly+j;
#ifdef DEBUG
	    printf("dx[%ld] = %g\n", xInd, dx[xInd]);
	    printf("dy[%ld] = %g\n", yInd, dy[yInd]);
#endif
            dy[yInd] += dx[xInd]*dS[i];
            iy[yInd] = 1;   /* keep track of where there are values */
          }
        }
      }
/*
 * Coerce output back to float or double at indices where there
 * are real values. (It's not enough to just check if dy==0.0,
 * because this could be a real value.)
 */
      coerce_output_float_or_double_ind(y,dy,type_y,nrowcoly,index_y,iy);
    }
/*
 * Clean up
 */
    NclFree(dx);
    NclFree(dy);
    NclFree(iy);
    NclFree(row);
    NclFree(col);
    if(type_S != NCL_double) NclFree(dS);

/*
 * Return value back to NCL script.
 */
    if(type_y == NCL_double) {
      ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_dx,type_y,0);
    }
    else {
      ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_fx,type_y,0);
    }
    NclFree(dsizes_y);
    return(ret);
}


NhlErrorTypes sparse_matrix_mult_trimesh_W
#if NhlNeedProto
(void)
#else
()
#endif
{
    /* Locally used variables */
    ng_size_t nvector, nmatrices, ntotal;
    ng_size_t ncoly, nrowy, nrowcoly, ncolx, nrowx, nrowcolx;
    ng_size_t i, j, l, index_x, index_y;
    ng_size_t xInd, yInd;
    ng_size_t *iy;
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
    int  ii, itri, ndims_x, has_missing_x;
    NclBasicDataTypes type_x;
    ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
    
    /* Argument # 4 */
    int   *trimesh;
    ng_size_t dsizes_trimesh[2];
    
    /* Argument # 5 */
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
                6,
                NULL,
                dsizes_row,
                NULL,
                NULL,
                &type_row,
                DONT_CARE);    

    /* Argument # 1 */
    col_in = (void*)NclGetArgValue(
                1,
                6,
                NULL,
                dsizes_col,
                NULL,
                NULL,
                &type_col,
                DONT_CARE);    


    /* Argument # 2 */
    S = (void*)NclGetArgValue(
                2,
                6,
                NULL,
                dsizes_S,
                NULL,
                NULL,
                &type_S,
                DONT_CARE); 

    if(dsizes_col[0] != dsizes_S[0] || dsizes_col[0] != dsizes_row[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: row, col, and S must have same number of elements");
        return(NhlFATAL);
    }

  /* Convert the row,col indexes to ng_size_t. */
    ndims = (int)dsizes_row[0];
    row = get_dimensions(row_in,ndims,type_row,"sparse_matrix_mult_trimesh");
    col = get_dimensions(col_in,ndims,type_col,"sparse_matrix_mult_trimesh");
    if(row == NULL || col == NULL) 
      return(NhlFATAL);

      /* Argument # 3 */
    x = (void*)NclGetArgValue(
                3,
                6,
                &ndims_x,
                dsizes_x,
                &missing_x,
                &has_missing_x,
                &type_x,
                DONT_CARE); 

      /* Argument # 3 */
    trimesh = (int*)NclGetArgValue(
                4,
                6,
                NULL,
                dsizes_trimesh,
                NULL,
                NULL,
                NULL,
                DONT_CARE); 

      /* Argument #4 */
    tmp_output_dsizes = (void*)NclGetArgValue(
           5,
           6,
           NULL,
           dsizes_output_dsizes,
           NULL,
           NULL,
           &type_output_dsizes,
           DONT_CARE);

    if(dsizes_output_dsizes[0] < 0 || dsizes_output_dsizes[0] > 2)  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: the output dimensions must represent a 1D or 2D array.");
      return(NhlFATAL);
    }


/* Convert the output dimensions to ng_size_t. */
    output_dsizes = get_dimensions(tmp_output_dsizes,dsizes_output_dsizes[0],
                                   type_output_dsizes,"sparse_matrix_mult_trimesh");

/* 
 * Get nrowy and ncoly for output array.
 *
 * This function handles a special case where the dimension 
 * sizes for the output array can represent a 1D array rather
 * than a 2D matrix. If it's a 1D array, then it will be 
 * treated as an [nrow,1] array.
 * 
 * If the output dims are for a 2D array, then x must be at
 * least a 2D array.
 */
    if(dsizes_output_dsizes[0] == 2) {
      if(ndims_x < 2) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: the input array must be at least 2-dimensional if the output dimensions represent a 2D array");
        return(NhlFATAL);
      }
      nrowx = dsizes_x[ndims_x-2];
      ncolx = dsizes_x[ndims_x-1];
      nrowy = output_dsizes[0];
      ncoly = output_dsizes[1];
    }
    else {
      nrowx = dsizes_x[ndims_x-1];
      ncolx = 1;
      nrowy = output_dsizes[0];
      ncoly = 1;
    }

    if(dsizes_trimesh[1] != 3)  {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: the trimesh array must be n x 3");
      return(NhlFATAL);
    }

#ifdef DEBUG
    printf("nrowx = %ld\n", nrowx);
    printf("ncolx = %ld\n", ncolx);
    printf("nrowy = %ld\n", nrowy);
    printf("ncoly = %ld\n", ncoly);
#endif
    if(ncolx != ncoly) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: input array is not the correct dimensionality");
      return(NhlFATAL);
    }
    nvector   = dsizes_S[0];
    nrowcoly  = nrowy * ncoly;
    nrowcolx  = nrowx * ncolx;

    printf("nvector =  %ld\n", nvector);
    printf("dsizes_trimesh = %ld %ld\n", dsizes_trimesh[0],dsizes_trimesh[1]);

    dsizes_y = (ng_size_t*)malloc(ndims_x*sizeof(ng_size_t));
    nmatrices = 1;
    if(dsizes_output_dsizes[0] == 2) {
      for(i = 0; i < ndims_x-2; i++) {
        nmatrices *= dsizes_x[i];
        dsizes_y[i] = dsizes_x[i];
      }
      dsizes_y[ndims_x-2] = nrowy;
      dsizes_y[ndims_x-1] = ncoly;
    }
    else {
      for(i = 0; i < ndims_x-1; i++) {
        nmatrices *= dsizes_x[i];
        dsizes_y[i] = dsizes_x[i];
      }
      dsizes_y[ndims_x-1] = nrowy;
    }
    ntotal = nmatrices * nrowcoly;
    printf("nmatrices = %ld\n", nmatrices);
    for(i = 0; i < ndims_x; i++) {
      printf("dsizes_y[%ld] =  %ld\n", i, dsizes_y[i]);
    }
#ifdef DEBUG
#endif

/*
 * Coerce missing values to double if necessary.
 */
    coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_fx);

/*
 * Coerce S to double if necessary.
 */
    dS = coerce_input_double(S,type_S,dsizes_S[0],0,NULL,NULL);
    if(dS == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: Unable to allocate memory for coercing input to double");
      return(NhlFATAL);
    }    
/*
 * Create temporary array for x no matter what, because we might need
 * to set values to missing.
 */
    dx = (double*)calloc(nrowcolx,sizeof(double));
    if(dx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: Unable to allocate memory for coercing input to double");
      return(NhlFATAL);
    }
/*
 * Create output variable. Create space for dy no matter what, because we need
 * to set it to zero, but y to missing.
 */
    if(type_x == NCL_double || type_S == NCL_double) {
      type_y = NCL_double;
      y      = (void*)calloc(ntotal,sizeof(double));
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
      type_y = NCL_float;
      y      = (void*)calloc(ntotal,sizeof(float));
      if(y == NULL) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    dy = (double*)calloc(nrowcoly,sizeof(double));
    iy = (ng_size_t*)calloc(nrowcoly,sizeof(ng_size_t));
    if(iy == NULL || dy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sparse_matrix_mult_trimesh: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }

/*
 * Set all output Y values to missing initially.
 */
    if(type_y == NCL_double) {
      for(i = 0; i < ntotal; i++) ((double*)y)[i] = missing_dx.doubleval;
    }
    else {
      for(i = 0; i < ntotal; i++) ((float*)y)[i] = missing_fx.floatval;
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

/* These arrays need to be zeroed out every time. */
      for (i = 0; i < nrowcoly; i++) {
        dy[i] = 0.0;
        iy[i] = 0;
      }

      if (has_missing_x) {
/*        
 * This is the loop for the calculation, with checks for missing x values.
 */
        for(i = 0; i < nvector; i++) {
	  for(itri=0;itri<3;itri++) {
	    ii = trimesh[col[i]*3+itri];
	    for (j = 0; j < ncoly; j++) {
	      xInd = ii*ncoly+j;
	      yInd = ii*ncoly+j;
	      if (dx[xInd] != missing_dx.doubleval) {
		dy[yInd] += dx[xInd]*(dS[i]/3.);
		iy[yInd] = 1;   /* keep track of where there are values */
	      }
            }
          }
        }
      }
      else {
/*
 * This is the loop for the calculation, with NO checks for missing x values.
 */
        for(i = 0; i < nvector; i++) {
	  for(itri=0;itri<3;itri++) {
	    ii = trimesh[col[i]*3+itri];
	    for (j = 0; j < ncoly; j++) {
	      xInd = ii*ncoly+j;
	      yInd = ii*ncoly+j;
	      dy[yInd] += dx[xInd]*(dS[i]/3.);
	      iy[yInd] = 1;   /* keep track of where there are values */
	    }
          }
        }
      }
/*
 * Coerce output back to float or double at indices where there
 * are real values. (It's not enough to just check if dy==0.0,
 * because this could be a real value.)
 */
      coerce_output_float_or_double_ind(y,dy,type_y,nrowcoly,index_y,iy);
    }
/*
 * Clean up
 */
    NclFree(dx);
    NclFree(dy);
    NclFree(iy);
    NclFree(row);
    NclFree(col);
    if(type_S != NCL_double) NclFree(dS);

/*
 * Return value back to NCL script.
 */
    if(type_y == NCL_double) {
      ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_dx,type_y,0);
    }
    else {
      ret = NclReturnValue(y,ndims_x,dsizes_y,&missing_fx,type_y,0);
    }
    NclFree(dsizes_y);
    return(ret);
}

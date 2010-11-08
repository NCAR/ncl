#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(poisxy1,POISXY1)(double *, int *, int *, double *, int *,
                                     int *, int *, double *, double *, int *,
                                     int *);

NhlErrorTypes poisson_grid_fill_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_dx;
  NclBasicDataTypes type_x;
/*
 * Argument # 1
 */
  logical *is_cyclic;
/*
 * Arguments # 2 & 3
 */
  int *guess_type, *nscan;
/*
 * Arguments # 4 & 5
 */
  void *epsx, *relc;
  double *tmp_epsx, *tmp_relc;
  NclBasicDataTypes type_epsx, type_relc;
/*
 * Argument # 6
 */
  int *opt;
/*
 * Various
 */
  int ndims_leftmost;
  ng_size_t i, size_leftmost;
  ng_size_t ny, mx, nymx, index_x;
  int mscan, ier, iny, imx;

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
           7,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           1);
/*
 * Check the input type.
 */
  if(type_x != NCL_float && type_x != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"poisson_grid_fill: The first input argument must be float or double");
    return(NhlFATAL);
  }

/*
 * Check dimension sizes.
 */
  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"poisson_grid_fill: The first argument must have at least two dimensions");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);

  ny   = dsizes_x[ndims_x-2];
  mx   = dsizes_x[ndims_x-1];
  nymx = ny * mx;

/*
 * Test input dimension sizes.
 */
    if((mx > INT_MAX) || (ny > INT_MAX)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"poisson_grid_fill: one or more input dimension sizes is greater than INT_MAX");
      return(NhlFATAL);
    }
    imx = (int) mx;
    iny = (int) ny;

/*
 * Get argument # 1
 */
  is_cyclic = (logical*)NclGetArgValue(
           1,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 2
 */
  guess_type = (int*)NclGetArgValue(
           2,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 3
 */
  nscan = (int*)NclGetArgValue(
           3,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 4
 */
  epsx = (void*)NclGetArgValue(
           4,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_epsx,
           DONT_CARE);
/*
 * Get argument # 4
 */
  relc = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_relc,
           DONT_CARE);
/*
 * Get argument # 6
 */
  opt = (int*)NclGetArgValue(
           6,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = ndims_x-2;
  for(i = 0; i < ndims_leftmost; i++) {
    size_leftmost *= dsizes_x[i];
  }

/*
 * Coerce the numeric input values to double.
 */
  tmp_epsx = coerce_input_double(epsx, type_epsx, 1, 0, NULL, NULL);
  tmp_relc = coerce_input_double(relc, type_relc, 1, 0, NULL, NULL);

/*
 * Allocate space for tmp_x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double *)calloc(nymx,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"poisson_grid_fill: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,nymx,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }
/*
 * Call the Fortran routine.
 */
    NGCALLF(poisxy1,POISXY1)(tmp_x, &imx, &iny, &missing_dx.doubleval, 
                             guess_type, is_cyclic, nscan, tmp_epsx,
                             tmp_relc, &mscan, &ier);
/*
 * Coerce back to float, if not double.
 */
    if(type_x == NCL_float) {
        coerce_output_float_only(x,tmp_x,nymx,index_x);
    }
    index_x += nymx;   /* Increment pointer. */
  }
/*
 * Free unneeded memory.
 */
 if(type_x    != NCL_double) NclFree(tmp_x);
 if(type_epsx != NCL_double) NclFree(tmp_epsx);
 if(type_relc != NCL_double) NclFree(tmp_relc);

/*
 * This is a procedure, so no values are returned.
 */
  return(NhlNOERROR);
}

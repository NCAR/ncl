#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dtrm,DTRM)(double *, int *, double *, int *);

NhlErrorTypes determinant_W( void )
{

/*
 * Input variable
 */
  void *x;
  double *tmp_x;
  ng_size_t dsizes_x[2];
  NclBasicDataTypes type_x;

/*
 * Return variable
 */
  void *det;
  double tmp_det;
  ng_size_t dsizes_det[1];
  NclBasicDataTypes type_det;

/*
 * Various
 */
  ng_size_t nx, nxnx;
  int ret, inx, *indx;

/*
 * Retrieve input.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);
  if(dsizes_x[0] != dsizes_x[1]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"determinant: input array must be a two-dimensional square matrix");
    return(NhlFATAL);
  }
  nx   = dsizes_x[0];
  nxnx = nx * nx;

/*
 * Test input dimension sizes.
 */
  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"determinant: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int) nx;

/*
 * The output type defaults to float, unless the input
 * array is double.
 */
  tmp_x = coerce_input_double(x,type_x,nxnx,0,NULL,NULL);
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"determinant: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
  if(type_x != NCL_double) {
    type_det = NCL_float;
    det = (void *)calloc(1, sizeof(float));
  }
  else {
    type_det = NCL_double;
    det = (void *)calloc(1, sizeof(double));
  }

/*
 * Allocate space for work array.
 */
  indx = (int *)calloc(nx, sizeof(int));
  if(indx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"determinant: Unable to allocate memory for work arary.");
    return(NhlFATAL);
  }

/*
 * Output is a scalar. Easy!
 */
  dsizes_det[0] = 1;

/*
 * Call the Fortran routine.
 */
  NGCALLF(dtrm,DTRM)(tmp_x, &inx, &tmp_det, indx);

/*
 * Coerce scalar to float or double
 */
  coerce_output_float_or_double(det,&tmp_det,type_det,1,0);

  if(type_x != NCL_double) NclFree(tmp_x);
  NclFree(indx);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(det,1,dsizes_det,NULL,type_det,0);
  return(ret);
}

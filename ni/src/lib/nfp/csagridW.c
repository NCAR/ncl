#include <string.h>
#include <stdio.h>
#include "wrapper.h"

char csamsg[61];

NhlErrorTypes csa1xs_W(void)
{
  int scalar_wts;
  ng_size_t i, j;
  ng_size_t size_output, size_leftmost;
  ng_size_t npts, nxo;
  int inpts, inxo, ret, ier = 0;
  ng_size_t index_xi = 0, index_yi = 0, index_yo = 0;
  float *xi;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  float *wts;
  ng_size_t dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  ng_size_t dsizes_xo[1];

  float *yo, *yo_tmp;
  ng_size_t *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 *
 * Note that the the csa1* arguments are a special case of where xi
 * can be multi-dimensional, OR 1-dimensional.
 */
  xi = (float *) NclGetArgValue(0, 7, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 7, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1xs: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xs: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1xs: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xs: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #2 (weights).
 */
  wts = (float *) NclGetArgValue(2, 7, NULL, dsizes_wts, NULL, 
                                 NULL, NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #2.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xs: Argument 2 must be the same length as argument 0 (if it is not a scalar)");
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (smoothing option).
 */
  smth = (float *) NclGetArgValue(4, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(5, 7, NULL, NULL, NULL, 
                                  NULL, NULL, DONT_CARE);
/*
 * Retrieve argument #6 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(6, 7, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1xs: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (float *) calloc(size_output, sizeof(float));
  dsizes_yo =   (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1xs(inpts, &xi[index_xi], &yi[index_yi], wts, 
                      *knots, *smth, *nderiv, inxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1xs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
    index_yo += nxo;
    free(yo_tmp);
  }
  ret = NclReturnValue( (void *) yo, ndims_yi, dsizes_yo, NULL, NCL_float, 0);
  NclFree(dsizes_yo);
  return(ret);

}

NhlErrorTypes csa1s_W(void)
{
  ng_size_t i, j;
  ng_size_t npts, nxo, size_output, size_leftmost;
  int inpts, inxo, ret, ier = 0;
  ng_size_t index_xi = 0, index_yi = 0, index_yo = 0;
  float *xi;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  ng_size_t dsizes_xo[1];

  float *yo, *yo_tmp;
  ng_size_t *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 4, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 4, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 

/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1s: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1s: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1s: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1s: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #2 (knots).
 */
  knots = (int *) NclGetArgValue(2, 4, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #3 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(3, 4, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1s: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (float *) calloc(size_output, sizeof(float));
  dsizes_yo =   (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1s(inpts, &xi[index_xi], &yi[index_yi], *knots, inxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1s: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
    index_yo += nxo;
    free(yo_tmp);
  }
  ret = NclReturnValue( (void *) yo, ndims_yi, dsizes_yo, NULL, NCL_float, 0);
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes csa2s_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo, ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];

  float *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2s: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2s: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
      return(NhlFATAL);
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2s: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  nxonyo = nxo*nyo;
 
/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxonyo;
  zo        = (float *) calloc(size_output, sizeof(float));

  ndims_zo  = ndims_zi + 1;
  dsizes_zo =   (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2s(inpts, xi, yi, &zi[index_in], knots, inxo, inyo, xo, yo,
                     &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2s: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxonyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxonyo;
    free(zo_tmp);
  }
  
  ret = NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                        NCL_float, 0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2xs_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo;
  int scalar_wts, ier = 0;
  ng_size_t index_in = 0, index_zo = 0, ret;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  ng_size_t dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];

  float *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xs: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2xs: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xs: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2xs: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nxonyo = nxo*nyo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxonyo;
  zo        = (float *) calloc(size_output, sizeof(float));

  ndims_zo  = ndims_zi + 1;
  dsizes_zo =   (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2xs(inpts, xi, yi, &zi[index_in], wts, knots, *smth, nderiv,
                      inxo, inyo, xo, yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2xs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxonyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxonyo;
    free(zo_tmp);
  }

  ret = NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_float, 0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2ls_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0, scalar_zo;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];

  float *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2ls: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2ls: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2ls: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 4 and 5.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Arguments #4 and 5 must be the same size.");
    return(NhlFATAL);
  }

  scalar_zo = is_scalar(1,dsizes_xo);
/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;
  zo        = (float *) calloc(size_output, sizeof(float));

  dsizes_zo =   (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2ls: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2ls(inpts, xi, yi, &zi[index_in], knots, inxo, xo, yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2ls: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxo;
    free(zo_tmp);
  }

  ret = NclReturnValue((void *) zo, ndims_zo, dsizes_zo, NULL, NCL_float,
                        0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2lxs_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_zo, scalar_wts, ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];

  float *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxs: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lxs: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 

/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);

  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxs: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lxs: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 7 and 8.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Arguments #7 and 8 must be the same size.");
    return(NhlFATAL);
  }

  scalar_zo = is_scalar(1,dsizes_xo);

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;
  zo        = (float *) calloc(size_output, sizeof(float));

  dsizes_zo =   (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2lxs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2lxs(inpts, xi, yi, &zi[index_in], wts, knots, *smth, nderiv,
                       inxo, xo, yo, &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2lxs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxo;
    free(zo_tmp);
  }
  ret = NclReturnValue((void *) zo,ndims_zo,dsizes_zo,NULL,NCL_float,0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa3xs_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  ng_size_t size_output, size_leftmost;
  int ret, scalar_wts, ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  ng_size_t dsizes_zi[1];
  float *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];
  float *zo;
  ng_size_t dsizes_zo[1];

  float *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3xs: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3xs: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);

/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #4.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3xs: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3xs: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
 
/*
 * Calculate space for output array and its dimension sizes.
 */

  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (float *) calloc(size_output, sizeof(float));

  ndims_uo  = ndims_ui + 2;
  dsizes_uo =   (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3xs(inpts, xi, yi, zi, 
                      &ui[index_in], wts, knots, *smth, nderiv,
                      inxo, inyo, inzo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3xs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxyz; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxyz;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_float, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3s_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  ng_size_t size_output, size_leftmost;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  ng_size_t dsizes_zi[1];
  float *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];
  float *zo;
  ng_size_t dsizes_zo[1];

  float *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3s: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3s: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3s: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
 
/*
 * Calculate space for output array and its dimension sizes.
 */
  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (float *) calloc(size_output, sizeof(float));

  ndims_uo  = ndims_ui + 2;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3s(inpts, xi, yi, zi, &ui[index_in], knots, inxo, inyo, 
                     inzo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3s: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxyz; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxyz;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL, 
                         NCL_float, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3lxs_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_uo, scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  ng_size_t dsizes_zi[1];
  float *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];
  float *zo;
  ng_size_t dsizes_zo[1];

  float *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lxs: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lxs: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #4.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lxs: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lxs: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 8 and 9.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Arguments #8 and 9 must be the same size.");
    return(NhlFATAL);
  }

  scalar_uo = is_scalar(1,dsizes_xo);

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 8 and 9.
 */
  if(nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Arguments #8 and 10 must be the same size.");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  uo        = (float *) calloc(size_output, sizeof(float));

  dsizes_uo =   (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3lxs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3lxs(inpts, xi, yi, zi, &ui[index_in], wts, 
                       knots, *smth, nderiv, inxo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3lxs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxo;
    free(uo_tmp);
  }

  ret = NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_float, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3ls_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int scalar_uo, ier = 0;
  ng_size_t index_in = 0, index_uo = 0, ret;

  float *xi;
  ng_size_t dsizes_xi[1];
  float *yi;
  ng_size_t dsizes_yi[1];
  float *zi;
  ng_size_t dsizes_zi[1];
  float *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  ng_size_t dsizes_xo[1];
  float *yo;
  ng_size_t dsizes_yo[1];
  float *zo;
  ng_size_t dsizes_zo[1];

  float *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3ls: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3ls: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3ls: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 5 and 6.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Arguments #5 and 6 must be the same size.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 5 and 7.
 */
  if(nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Arguments #5 and 7 must be the same size.");
    return(NhlFATAL);
  }

  scalar_uo = is_scalar(1,dsizes_xo);

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  uo        = (float *) calloc(size_output, sizeof(float));

  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3ls: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3ls(inpts, xi, yi, zi, &ui[index_in], knots, inxo, xo, yo, 
                      zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3ls: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxo;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL,
                         NCL_float, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa1xd_W(void)
{
  ng_size_t i, j, npts, nxo;
  int inpts, inxo;
  int scalar_wts;
  ng_size_t size_output, size_leftmost;
  int ret, ier = 0;
  ng_size_t index_xi = 0, index_yi = 0, index_yo = 0;
  double *xi;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  double *yi;
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  double *wts;
  ng_size_t dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  double *smth;
  int *nderiv;
  double *xo;
  ng_size_t dsizes_xo[1];

  double *yo, *yo_tmp;
  ng_size_t *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 7, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 7, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xd: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1xd: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xd: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1xd: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (weights).
 */
  wts = (double *) NclGetArgValue(2, 7, NULL, dsizes_wts, NULL, 
                                 NULL, NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #2.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xd: Argument 2 must be the same length as argument 0 (if it is not a scalar)");
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (smoothing option).
 */
  smth = (double *) NclGetArgValue(4, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(5, 7, NULL, NULL, NULL, 
                                  NULL, NULL, DONT_CARE);
/*
 * Retrieve argument #6 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(6, 7, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1xd: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (double *) calloc(size_output, sizeof(double));
  dsizes_yo = (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1xd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1xd(inpts, &xi[index_xi], &yi[index_yi], wts, 
                      *knots, *smth, *nderiv, inxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
    index_yo += nxo;
    free(yo_tmp);
  }
  ret = NclReturnValue( (void *) yo, ndims_yi, dsizes_yo, NULL, NCL_double, 0);
  NclFree(dsizes_yo);
  return(ret);

}

NhlErrorTypes csa1d_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret, ier = 0;
  ng_size_t index_xi = 0, index_yi = 0, index_yo = 0;

  double *xi;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  double *yi;
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  int *knots;
  double *xo;
  ng_size_t dsizes_xo[1];

  double *yo, *yo_tmp;
  ng_size_t *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 4, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 4, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1d: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1d: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1d: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1d: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #2 (knots).
 */
  knots = (int *) NclGetArgValue(2, 4, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #3 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(3, 4, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1d: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (double *) calloc(size_output, sizeof(double));
  dsizes_yo = (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1d(inpts, &xi[index_xi], &yi[index_yi], *knots, inxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
    index_yo += nxo;
    free(yo_tmp);
  }
  ret = NclReturnValue( (void *) yo, ndims_yi, dsizes_yo, NULL, NCL_double, 0);
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes csa2d_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];

  double *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2d: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2d: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2d: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
      return(NhlFATAL);
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2d: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  nxonyo = nxo*nyo;
 
/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxonyo;
  zo        = (double *) calloc(size_output, sizeof(double));

  ndims_zo  = ndims_zi + 1;
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2d(inpts, xi, yi, &zi[index_in], knots, inxo, inyo, xo, yo,
                     &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxonyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxonyo;
    free(zo_tmp);
  }
  
  ret = NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_double, 0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2xd_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo;
  int scalar_wts, ier = 0;
  ng_size_t index_in = 0, index_zo = 0, ret;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  double *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  double *smth;
  int *nderiv;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];

  double *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xd: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2xd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xd: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (double *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xd: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (double *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2xd: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nxonyo = nxo*nyo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxonyo;
  zo        = (double *) calloc(size_output, sizeof(double));

  ndims_zo  = ndims_zi + 1;
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2xd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2xd(inpts, xi, yi, &zi[index_in], wts, knots, *smth, nderiv,
                      inxo, inyo, xo, yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxonyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxonyo;
    free(zo_tmp);
  }

  ret = NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_double, 0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2ld_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0, scalar_zo;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];

  double *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2ld: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2ld: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ld: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2ld: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 4 and 5.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ld: Arguments #4 and 5 must be the same size.");
    return(NhlFATAL);
  }

  scalar_zo = is_scalar(1,dsizes_xo);
/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;
  zo        = (double *) calloc(size_output, sizeof(double));

  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2ld: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2ld(inpts, xi, yi, &zi[index_in], knots, inxo, xo, yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2ld: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxo;
    free(zo_tmp);
  }

  ret = NclReturnValue((void *) zo, ndims_zo, dsizes_zo, NULL, NCL_double,
                        0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa2lxd_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_zo, scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  double *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  double *smth;
  int *nderiv;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];

  double *zo, *zo_tmp;
  int ndims_zo;
  ng_size_t *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxd: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lxd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 

/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxd: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (double *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);

  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxd: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (double *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lxd: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 7 and 8.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxd: Arguments #7 and 8 must be the same size.");
    return(NhlFATAL);
  }

  scalar_zo = is_scalar(1,dsizes_xo);

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;
  zo        = (double *) calloc(size_output, sizeof(double));

  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2lxd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2lxd(inpts, xi, yi, &zi[index_in], wts, knots, *smth, nderiv,
                       inxo, xo, yo, &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2lxd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nxo;
    free(zo_tmp);
  }
  ret = NclReturnValue((void *) zo,ndims_zo,dsizes_zo,NULL,NCL_double,0);
  NclFree(dsizes_zo);
  return(ret);

}

NhlErrorTypes csa3xd_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  int ret;
  ng_size_t size_output, size_leftmost;
  int scalar_wts, ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  ng_size_t dsizes_zi[1];
  double *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  double *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  double *smth;
  int *nderiv;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];
  double *zo;
  ng_size_t dsizes_zo[1];

  double *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (double *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3xd: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3xd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);

/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xd: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (double *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #4.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3xd: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (double *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (double *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3xs: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;

/*
 * Calculate space for output array and its dimension sizes.
 */

  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (double *) calloc(size_output, sizeof(double));

  ndims_uo  = ndims_ui + 2;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3xd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3xd(inpts, xi, yi, zi, 
                      &ui[index_in], wts, knots, *smth, nderiv,
                      inxo, inyo, inzo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxyz; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxyz;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_double, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3d_W(void)
{
  ng_size_t i, j, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  ng_size_t size_output, size_leftmost;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  ng_size_t dsizes_zi[1];
  double *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];
  double *zo;
  ng_size_t dsizes_zo[1];

  double *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (double *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3d: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3d: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3d: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (double *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3xs: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
 
/*
 * Calculate space for output array and its dimension sizes.
 */
  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (double *) calloc(size_output, sizeof(double));

  ndims_uo  = ndims_ui + 2;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3d: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3d(inpts, xi, yi, zi, &ui[index_in], knots, inxo, inyo, 
                     inzo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxyz; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxyz;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL, 
                         NCL_double, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3lxd_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_uo, scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  ng_size_t dsizes_zi[1];
  double *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  double *wts;
  ng_size_t dsizes_wts[1];
  int *knots;
  double *smth;
  int *nderiv;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];
  double *zo;
  ng_size_t dsizes_zo[1];

  double *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (double *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lxd: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lxd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxd: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (double *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                 NULL, DONT_CARE);
 
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for arguments #4.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lxd: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (double *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lxd: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 8 and 9.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxd: Arguments #8 and 9 must be the same size.");
    return(NhlFATAL);
  }

  scalar_uo = is_scalar(1,dsizes_xo);

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (double *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 8 and 9.
 */
  if(nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxd: Arguments #8 and 10 must be the same size.");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  uo        = (double *) calloc(size_output, sizeof(double));

  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3lxd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3lxd(inpts, xi, yi, zi, &ui[index_in], wts, 
                       knots, *smth, nderiv, inxo, xo, yo, zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3lxd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxo;
    free(uo_tmp);
  }

  ret = NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_double, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa3ld_W(void)
{
  ng_size_t i, j, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int scalar_uo, ier = 0;
  ng_size_t index_in = 0, index_uo = 0, ret;

  double *xi;
  ng_size_t dsizes_xi[1];
  double *yi;
  ng_size_t dsizes_yi[1];
  double *zi;
  ng_size_t dsizes_zi[1];
  double *ui;
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  double *xo;
  ng_size_t dsizes_xo[1];
  double *yo;
  ng_size_t dsizes_yo[1];
  double *zo;
  ng_size_t dsizes_zo[1];

  double *uo, *uo_tmp;
  int ndims_uo;
  ng_size_t *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (double *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (double *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3ld: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3ld: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ld: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3ld: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (double *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 5 and 6.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ld: Arguments #5 and 6 must be the same size.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (double *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Check sizes of arguments 5 and 7.
 */
  if(nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ld: Arguments #5 and 7 must be the same size.");
    return(NhlFATAL);
  }

  scalar_uo = is_scalar(1,dsizes_xo);

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  uo        = (double *) calloc(size_output, sizeof(double));

  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3ld: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3ld(inpts, xi, yi, zi, &ui[index_in], knots, inxo, xo, yo, 
                      zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3ld: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(uo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      uo[index_uo+j] = uo_tmp[j];
    }
    index_in += npts;
    index_uo += nxo;
    free(uo_tmp);
  }
  ret = NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL,
                         NCL_double, 0);
  NclFree(dsizes_uo);
  return(ret);

}

NhlErrorTypes csa1x_W(void)
{
  void *xi, *yi, *wts, *smth, *xo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wts[1], dsizes_xo[1];
  int *knots, *nderiv;
  NclBasicDataTypes type_xi, type_yi, type_wts, type_smth, type_xo;
  double *wts_val, *tmp_wts, *tmp_smth, *tmp_xo;
  double *tmp_xi = NULL;
  double *tmp_yi = NULL;
/*
 * Various. 
 */
  ng_size_t i, npts, nxo;
  int inpts, inxo;
  int scalar_wts;
  ng_size_t size_output, size_leftmost;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_yo = 0;
/*
 * Output variables.
 */
  void *yo;
  double *tmp_yo;
  ng_size_t *dsizes_yo;
  NclBasicDataTypes type_yo;

/*
 * Retrieve arguments #0 and 1.
 */
  xi = (void *) NclGetArgValue(0, 7, &ndims_xi, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 7, &ndims_yi, dsizes_yi, NULL, NULL, 
                               &type_yi, DONT_CARE);
 
/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1x: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1x: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1x: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Create temp arrays for coercing xi and yi to double if necessary.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(npts,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing xi array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_yi != NCL_double) {
    tmp_yi = (double*)calloc(npts,sizeof(double));
    if(tmp_yi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing yi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Retrieve argument #2 (weights).
 */
  wts = (void *) NclGetArgValue(2, 7, NULL, dsizes_wts, NULL, 
                                NULL, &type_wts, DONT_CARE);
 
/*
 * wts can be a scalar or one-dimensional. If it is a scalar, 
 * then we need to construct an npts-sized wts array that 
 * is filled with the scalar value.
 */
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for wts.
 */
  if(!scalar_wts && dsizes_wts[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1x: Argument 2 must be the same length as argument 0 (if it is not a scalar)");
  }
/*
 * If scalar, then first coerce the scalar to double precision.  Then,
 * copy it to an npts array. 
 */
  if(scalar_wts) {
    wts_val = coerce_input_double(wts,type_wts,1,0,NULL,NULL);
    tmp_wts = (double*)calloc(npts,sizeof(double));
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
    for(i = 0; i < npts; i++ ) tmp_wts[i] = *wts_val;
    if (wts_val != wts)
      NclFree(wts_val);
  }
  else {
    tmp_wts = coerce_input_double(wts,type_wts,npts,0,NULL,NULL);
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
  }
      
/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #4 (smoothing option) and coerce to double if
 * necessary.
 */
  smth     = (void *) NclGetArgValue(4, 7, NULL, NULL, NULL, NULL, 
                                     &type_smth, DONT_CARE);
  tmp_smth = coerce_input_double(smth,type_smth,1,0,NULL,NULL);

  if(tmp_smth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing smth array to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #5 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(5, 7, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
/*
 * Retrieve argument #6 (output x coordinates).
 */
  xo = (void *) NclGetArgValue(6, 7, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1x: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }
 
/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxo;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_xo == NCL_double) {
    type_yo = NCL_double;
    yo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_yo = NCL_float;
    yo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_yo = (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1x: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_xi > 1 || i == 0) {
      if(type_xi != NCL_double) {
/*
 * Coerce npts subsection of xi (tmp_xi) to double.
 */
        coerce_subset_input_double(xi,tmp_xi,index_in,type_xi,npts,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_xi to appropriate location in xi.
 */
        tmp_xi = &((double*)xi)[index_in];
      }
    }

    if(type_yi != NCL_double) {
/*
 * Coerce npts subsection of yi (tmp_yi) to double.
 */
      coerce_subset_input_double(yi,tmp_yi,index_in,type_yi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_yi to appropriate location in yi.
 */
      tmp_yi = &((double*)yi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_yo = c_csa1xd(inpts, tmp_xi, tmp_yi, tmp_wts, 
                      *knots, *tmp_smth, *nderiv, inxo, tmp_xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_yo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(yo,tmp_yo,type_yo,nxo,index_yo);

    index_in += npts;
    index_yo += nxo;
    free(tmp_yo);
  }
  if(type_xi   != NCL_double)               NclFree(tmp_xi);
  if(type_yi   != NCL_double)               NclFree(tmp_yi);
  if(type_wts  != NCL_double || scalar_wts) NclFree(tmp_wts);
  if(type_smth != NCL_double)               NclFree(tmp_smth);
  if(type_xo   != NCL_double)               NclFree(tmp_xo);

  ret = NclReturnValue(yo, ndims_yi, dsizes_yo, NULL, type_yo, 0);
  NclFree(dsizes_yo);
  return(ret);

}


NhlErrorTypes csa1_W(void)
{
  void *xi, *yi, *xo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  int *knots;
  ng_size_t dsizes_xo[1];
  NclBasicDataTypes type_xi, type_yi, type_xo;
  double *tmp_xi = NULL;
  double *tmp_yi = NULL;
  double *tmp_xo;
/*
 * Various. 
 */
  ng_size_t i, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_yo = 0;
/*
 * Output variables.
 */
  void *yo;
  double *tmp_yo;
  ng_size_t *dsizes_yo;
  NclBasicDataTypes type_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (void *) NclGetArgValue(0, 4, &ndims_xi, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
 
  yi = (void *) NclGetArgValue(1, 4, &ndims_yi, dsizes_yi, NULL, NULL, 
                               &type_yi, DONT_CARE);
 
/*
 * Either the dimensions of xi and yi must be the same, or if xi
 * is 1D and yi is nD, then the last dimension of yi must be equal
 * to the dimension of xi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
      return(NhlFATAL);
    }
    else {
      for( i = 0; i < ndims_xi; i++ ) {
        if(dsizes_xi[i] != dsizes_yi[i]) {
          NhlPError(NhlFATAL, NhlEUNKNOWN,
                    "csa1: If arguments #0 and 1 are multi-dimensional, they must have the same dimensions sizes.");
          return(NhlFATAL);
        }
      }
    }
  }
  else {
    if(dsizes_xi[0] != dsizes_yi[ndims_yi-1]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1: If argument #0 is one-dimensional, it must be the same as the rightmost dimension of argument #1.");
      return(NhlFATAL);
    }
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_yi[ndims_yi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1: the rightmost dimension of yi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_yi[ndims_yi-1];
  inpts = (int) npts;

/*
 * Create temp arrays for coercing xi and yi to double if ncessary.
 */
  if(type_xi != NCL_double) {
    tmp_xi = (double*)calloc(npts,sizeof(double));
    if(tmp_xi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1: Unable to allocate memory for coercing xi array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_yi != NCL_double) {
    tmp_yi = (double*)calloc(npts,sizeof(double));
    if(tmp_yi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1: Unable to allocate memory for coercing yi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Retrieve argument #2 (knots).
 */
  knots = (int *) NclGetArgValue(2, 4, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #3 (output x coordinates).
 */
  xo = (void *) NclGetArgValue(3, 4, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa1: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxo;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_xo == NCL_double) {
    type_yo = NCL_double;
    yo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_yo = NCL_float;
    yo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_yo = (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(ndims_xi > 1 || i == 0) {
      if(type_xi != NCL_double) {
/*
 * Coerce npts subsection of xi (tmp_xi) to double.
 */
        coerce_subset_input_double(xi,tmp_xi,index_in,type_xi,npts,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_xi to appropriate location in xi.
 */
        tmp_xi = &((double*)xi)[index_in];
      }
    }

    if(type_yi != NCL_double) {
/*
 * Coerce npts subsection of yi (tmp_yi) to double.
 */
      coerce_subset_input_double(yi,tmp_yi,index_in,type_yi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_yi to appropriate location in yi.
 */
      tmp_yi = &((double*)yi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_yo = c_csa1d(inpts, tmp_xi, tmp_yi, *knots, inxo, tmp_xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_yo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(yo,tmp_yo,type_yo,nxo,index_yo);

    index_in += npts;
    index_yo += nxo;
    free(tmp_yo);
  }
  if(type_xi   != NCL_double) NclFree(tmp_xi);
  if(type_yi   != NCL_double) NclFree(tmp_yi);
  if(type_xo   != NCL_double) NclFree(tmp_xo);

  ret = NclReturnValue(yo, ndims_yi, dsizes_yo, NULL, type_yo, 0);
  NclFree(dsizes_yo);
  return(ret);


}

NhlErrorTypes csa2_W(void)
{
  void *xi, *yi, *zi, *xo, *yo;
  int *knots;
  ng_size_t dsizes_xi[1];
  ng_size_t dsizes_yi[1];
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_xo, type_yo;
  double *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo;
  double *tmp_zi = NULL;

/*
 * Various. 
 */
  ng_size_t i, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

/*
 * Output variables.
 */
  void *zo;
  double *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  NclBasicDataTypes type_zo;

/*
 * Retrieve argument #0 and 1 (x/y coordinates).
 */
  xi = (void *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL,
                               &type_yi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2: Unable to allocate memory for coercing xi/yi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (void *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                               &type_zi, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
      return(NhlFATAL);
  }

/*
 * Create temporary zi array.
 */ 
  if(type_zi != NCL_double) {
    tmp_zi = (double*)calloc(npts,sizeof(double));
    if(tmp_zi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2: Unable to allocate memory for coercing zi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve arguments #4 and 5 (output x/y coordinates).
 */
  xo = (void *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, 
                               &type_yo, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  nxonyo = nxo*nyo;
 
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxonyo;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double) {
    type_zo = NCL_double;
    zo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_zo = NCL_float;
    zo      = (void *) calloc(size_output, sizeof(float));
  }

  ndims_zo  = ndims_zi + 1;
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_zi != NCL_double) {
/*
 * Coerce npts subsection of zi (tmp_zi) to double.
 */
      coerce_subset_input_double(zi,tmp_zi,index_in,type_zi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zi to appropriate location in zi.
 */
      tmp_zi = &((double*)zi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_zo = c_csa2d(inpts, tmp_xi, tmp_yi, tmp_zi, knots, inxo, nyo, 
                     tmp_xo, tmp_yo, &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_zo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(zo,tmp_zo,type_zo,nxonyo,index_zo);

    index_in += npts;
    index_zo += nxonyo;
    free(tmp_zo);
  }
  if(type_xi   != NCL_double) NclFree(tmp_xi);
  if(type_yi   != NCL_double) NclFree(tmp_yi);
  if(type_zi   != NCL_double) NclFree(tmp_zi);
  if(type_xo   != NCL_double) NclFree(tmp_xo);
  if(type_yo   != NCL_double) NclFree(tmp_yo);
  
  ret = NclReturnValue( zo,  ndims_zo, dsizes_zo, NULL, type_zo, 0);
  NclFree(dsizes_zo);
  return(ret);

}


NhlErrorTypes csa2x_W(void)
{
  void *xi, *yi, *zi, *wts, *smth, *xo, *yo;
  ng_size_t dsizes_xi[1], dsizes_yi[1];
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wts[1];
  int *knots;
  int *nderiv;
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_wts, type_smth;
  NclBasicDataTypes type_xo, type_yo;
  double *tmp_xi, *tmp_yi, *wts_val, *tmp_wts, *tmp_smth;
  double *tmp_xo, *tmp_yo;
  double *tmp_zi = NULL;
/*
 * Various. 
 */
  ng_size_t i, npts, nxo, nyo, nxonyo, size_output, size_leftmost;
  int inpts, inxo, inyo;
  int scalar_wts, ier = 0, ret;
  ng_size_t index_in = 0, index_zo = 0;
/*
 * Output variables.
 */
  void *zo;
  double *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  NclBasicDataTypes type_zo;

/*
 * Retrieve arguments #0 and 1 (x/y coordinates).
 */
  xi = (void *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL,
                               &type_yi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2x: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing xi/yi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (void *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                               &type_zi, DONT_CARE);
 
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2x: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Create temporary zi array.
 */ 
  if(type_zi != NCL_double) {
    tmp_zi = (double*)calloc(npts,sizeof(double));
    if(tmp_zi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing zi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (void *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                &type_wts, DONT_CARE);
/*
 * wts can be a scalar or one-dimensional. If it is a scalar, 
 * then we need to construct an npts-sized wts array that 
 * is filled with the scalar value.
 */
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for wts.
 */
  if(!scalar_wts && dsizes_wts[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2x: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
  }

/*
 * If scalar, then first coerce the scalar to double precision.  Then,
 * copy it to an npts array. 
 */
  if(scalar_wts) {
    wts_val = coerce_input_double(wts,type_wts,1,0,NULL,NULL);
    tmp_wts = (double*)calloc(npts,sizeof(double));
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
    for(i = 0; i < npts; i++ ) tmp_wts[i] = *wts_val;
    if (wts_val != wts)
      NclFree(wts_val);
  }
  else {
    tmp_wts = coerce_input_double(wts,type_wts,npts,0,NULL,NULL);
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
  }
      
/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option) and coerce if necessary.
 */
  smth = (void *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, &type_smth, DONT_CARE);
  tmp_smth = coerce_input_double(smth,type_smth,1,0,NULL,NULL);

  if(tmp_smth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing smth array to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve arguments #7 and 8 (output x/y coordinates).
 */
  xo = (void *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL,
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL,
                               &type_yo, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: the length of xo and/or yo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;

  nxonyo = nxo*nyo;

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2x: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxonyo;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double) {
    type_zo = NCL_double;
    zo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_zo = NCL_float;
    zo      = (void *) calloc(size_output, sizeof(float));
  }

  ndims_zo  = ndims_zi + 1;
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2x: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_zi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_zi != NCL_double) {
/*
 * Coerce npts subsection of zi (tmp_zi) to double.
 */
      coerce_subset_input_double(zi,tmp_zi,index_in,type_zi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zi to appropriate location in zi.
 */
      tmp_zi = &((double*)zi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_zo = c_csa2xd(inpts, tmp_xi, tmp_yi, tmp_zi, tmp_wts, knots, 
                      *tmp_smth, nderiv, inxo, inyo, tmp_xo, tmp_yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_zo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(zo,tmp_zo,type_zo,nxonyo,index_zo);

    index_in += npts;
    index_zo += nxonyo;
    free(tmp_zo);
  }

  if(type_xi   != NCL_double)               NclFree(tmp_xi);
  if(type_yi   != NCL_double)               NclFree(tmp_yi);
  if(type_zi   != NCL_double)               NclFree(tmp_zi);
  if(type_wts  != NCL_double || scalar_wts) NclFree(tmp_wts);
  if(type_smth != NCL_double)               NclFree(tmp_smth);
  if(type_xo   != NCL_double)               NclFree(tmp_xo);
  if(type_yo   != NCL_double)               NclFree(tmp_yo);
  
  ret = NclReturnValue( zo,  ndims_zo, dsizes_zo, NULL, type_zo, 0);
  NclFree(dsizes_zo);
  return(ret);

}


NhlErrorTypes csa2l_W(void)
{
  void *xi, *yi, *zi, *xo, *yo;
  int *knots;
  ng_size_t dsizes_xi[1];
  ng_size_t dsizes_yi[1];
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_xo, type_yo;
  double *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo;
  double *tmp_zi = NULL;

/*
 * Various. 
 */
  ng_size_t i, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_zo = 0, scalar_zo;

/*
 * Output variables.
 */
  void *zo;
  double *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  NclBasicDataTypes type_zo;

/*
 * Retrieve arguments #0 and 1 (x/y coordinates).
 */
  xi = (void *) NclGetArgValue(0, 6, NULL, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 6, NULL, dsizes_yi, NULL, NULL,
                               &type_yi, DONT_CARE);

/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2l: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2l: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2l: Unable to allocate memory for coercing xi/yi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                 &type_zi, DONT_CARE);
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2l: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }

/*
 * Create temporary zi array.
 */ 
  if(type_zi != NCL_double) {
    tmp_zi = (double*)calloc(npts,sizeof(double));
    if(tmp_zi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2l: Unable to allocate memory for coercing zi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve arguments #4 and 5 (output x/y coordinates).
 */
  xo = (void *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, 
                               &type_yo, DONT_CARE);
/*
 * Check sizes of arguments 4 and 5.
 */
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2l: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2l: Arguments #4 and 5 must be the same size.");
    return(NhlFATAL);
  }
 
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2l: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  scalar_zo   = is_scalar(1,dsizes_xo);
  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;

  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double) {
    type_zo = NCL_double;
    zo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_zo = NCL_float;
    zo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2l: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_zi != NCL_double) {
/*
 * Coerce npts subsection of zi (tmp_zi) to double.
 */
      coerce_subset_input_double(zi,tmp_zi,index_in,type_zi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zi to appropriate location in zi.
 */
      tmp_zi = &((double*)zi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_zo = c_csa2ld(inpts, tmp_xi, tmp_yi, tmp_zi, knots, inxo, 
                      tmp_xo, tmp_yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2ld: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_zo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(zo,tmp_zo,type_zo,nxo,index_zo);

    index_in += npts;
    index_zo += nxo;
    free(tmp_zo);
  }
  if(type_xi   != NCL_double) NclFree(tmp_xi);
  if(type_yi   != NCL_double) NclFree(tmp_yi);
  if(type_zi   != NCL_double) NclFree(tmp_zi);
  if(type_xo   != NCL_double) NclFree(tmp_xo);
  if(type_yo   != NCL_double) NclFree(tmp_yo);
  
  ret = NclReturnValue( zo,  ndims_zo, dsizes_zo, NULL, type_zo, 0);
  NclFree(dsizes_zo);
  return(ret);

}


NhlErrorTypes csa2lx_W(void)
{
  void *xi, *yi, *zi, *wts, *smth, *xo, *yo;
  ng_size_t dsizes_xi[1], dsizes_yi[1];
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wts[1];
  int *knots, *nderiv;
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_wts, type_smth;
  NclBasicDataTypes type_xo, type_yo;
  double *tmp_xi, *tmp_yi, *wts_val, *tmp_wts, *tmp_smth;
  double *tmp_xo, *tmp_yo;
  double *tmp_zi = NULL;
/*
 * Various. 
 */
  ng_size_t i, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_zo, scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_zo = 0;

/*
 * Output variables.
 */
  void *zo;
  double *tmp_zo;
  int ndims_zo;
  ng_size_t *dsizes_zo;
  NclBasicDataTypes type_zo;

/*
 * Retrieve arguments #0 and 1 (x/y coordinates).
 */
  xi = (double *) NclGetArgValue(0, 9, NULL, dsizes_xi, NULL, NULL, 
                                 &type_xi, DONT_CARE);
  yi = (double *) NclGetArgValue(1, 9, NULL, dsizes_yi, NULL, NULL,
                                 &type_yi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(dsizes_xi[0] != dsizes_yi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lx: Arguments #0 and 1 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing xi/yi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (z values).
 */
  zi = (double *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                 &type_zi, DONT_CARE);
/*
 * Check last dimension of argument #2.
 */
  if(dsizes_zi[ndims_zi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lx: The last (rightmost) dimension of argument #2 must be the same length as arguments 0 and 1");
    return(NhlFATAL);
  }
/*
 * Create temporary zi array.
 */ 
  if(type_zi != NCL_double) {
    tmp_zi = (double*)calloc(npts,sizeof(double));
    if(tmp_zi == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing zi array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_zi-1; i++ ) size_leftmost *= dsizes_zi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (double *) NclGetArgValue(3, 9, NULL, dsizes_wts, NULL, NULL, 
                                  &type_wts, DONT_CARE);
/*
 * wts can be a scalar or one-dimensional. If it is a scalar, 
 * then we need to construct an npts-sized wts array that 
 * is filled with the scalar value.
 */
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for wts.
 */
  if(!scalar_wts) {
    if(dsizes_wts[0] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lx: Argument 3 must be the same length as argument 0 (if it is not a scalar)");
    }
  }

/*
 * If scalar, then first coerce the scalar to double precision.  Then,
 * copy it to an npts array. 
 */
  if(scalar_wts) {
    wts_val = coerce_input_double(wts,type_wts,1,0,NULL,NULL);
    tmp_wts = (double*)calloc(npts,sizeof(double));
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
    for(i = 0; i < npts; i++ ) tmp_wts[i] = *wts_val;
    if (wts_val != wts) 
      NclFree(wts_val);
  }
  else {
    tmp_wts = coerce_input_double(wts,type_wts,npts,0,NULL,NULL);
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
  }
      
/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (smoothing option) and coerce if necessary.
 */
  smth = (double *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, 
                                   &type_smth, DONT_CARE);
  tmp_smth = coerce_input_double(smth,type_smth,1,0,NULL,NULL);

  if(tmp_smth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing smth array to double precision");
    return(NhlFATAL);
  }
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve arguments #7 and 8 (output x/ coordinates).
 */
  xo = (void *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL,
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL,
                               &type_yo, DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Check sizes of arguments 7 and 8.
 */
  if(nxo != dsizes_yo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lx: Arguments #7 and 8 must be the same size.");
    return(NhlFATAL);
  }

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa2lx: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  scalar_zo = is_scalar(1,dsizes_xo);

  size_output = size_leftmost * nxo;
  if(scalar_zo && ndims_zi > 1) ndims_zo = ndims_zi-1;
  else                          ndims_zo = ndims_zi;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double) {
    type_zo = NCL_double;
    zo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_zo = NCL_float;
    zo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_zo = (ng_size_t *) calloc(ndims_zo, sizeof(ng_size_t));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2lx: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zi-1; i++ ) dsizes_zo[i] = dsizes_zi[i];
  if(!scalar_zo || (scalar_zo && ndims_zi == 1)) dsizes_zo[ndims_zi-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_zi != NCL_double) {
/*
 * Coerce npts subsection of zi (tmp_zi) to double.
 */
      coerce_subset_input_double(zi,tmp_zi,index_in,type_zi,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zi to appropriate location in zi.
 */
      tmp_zi = &((double*)zi)[index_in];
    }
/*
 * Call C function.
 */
    tmp_zo = c_csa2lxd(inpts, tmp_xi, tmp_yi, tmp_zi, tmp_wts, knots,
                       *tmp_smth, nderiv, inxo, tmp_xo, tmp_yo, &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2lxd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_zo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(zo,tmp_zo,type_zo,nxo,index_zo);

    index_in += npts;
    index_zo += nxo;
    free(tmp_zo);
  }

  if(type_xi   != NCL_double)               NclFree(tmp_xi);
  if(type_yi   != NCL_double)               NclFree(tmp_yi);
  if(type_zi   != NCL_double)               NclFree(tmp_zi);
  if(type_wts  != NCL_double || scalar_wts) NclFree(tmp_wts);
  if(type_smth != NCL_double)               NclFree(tmp_smth);
  if(type_xo   != NCL_double)               NclFree(tmp_xo);
  if(type_yo   != NCL_double)               NclFree(tmp_yo);
  
  ret = NclReturnValue( zo,  ndims_zo, dsizes_zo, NULL, type_zo, 0);
  NclFree(dsizes_zo);
  return(ret);

}


NhlErrorTypes csa3x_W(void)
{
  void *xi, *yi, *zi, *ui, *wts, *smth, *xo, *yo, *zo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_zi[1];
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wts[1];
  int *knots, *nderiv;
  ng_size_t dsizes_xo[1], dsizes_yo[1], dsizes_zo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_wts, type_smth;
  NclBasicDataTypes type_xo, type_yo, type_zo;
  double *tmp_xi, *tmp_yi, *tmp_zi, *wts_val, *tmp_wts, *tmp_smth;
  double *tmp_xo, *tmp_yo, *tmp_zo;
  double *tmp_ui = NULL;

/*
 * Various. 
 */
  ng_size_t i, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  ng_size_t size_output, size_leftmost;
  int scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_uo = 0, ret;

/*
 * Output variables.
 */
  void *uo;
  double *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  NclBasicDataTypes type_uo;

/*
 * Retrieve arguments #0, 1, 2 (x/y/z coordinates).
 */
  xi = (void *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                               &type_yi, DONT_CARE);
  zi = (void *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                               &type_zi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3x: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);
  tmp_zi = coerce_input_double(zi,type_zi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL || tmp_zi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing xi/yi/zi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (void *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                               &type_ui, DONT_CARE);
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3x: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Create temporary ui array.
 */ 
  if(type_ui != NCL_double) {
    tmp_ui = (double*)calloc(npts,sizeof(double));
    if(tmp_ui == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing ui array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (void *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                &type_wts, DONT_CARE);
 
/*
 * wts can be a scalar or one-dimensional. If it is a scalar, 
 * then we need to construct an npts-sized wts array that 
 * is filled with the scalar value.
 */
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for wts.
 */
  if(!scalar_wts && dsizes_wts[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3x: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
  }

/*
 * If scalar, then first coerce the scalar to double precision.  Then,
 * copy it to an npts array. 
 */
  if(scalar_wts) {
    wts_val = coerce_input_double(wts,type_wts,1,0,NULL,NULL);
    tmp_wts = (double*)calloc(npts,sizeof(double));
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
    for(i = 0; i < npts; i++ ) tmp_wts[i] = *wts_val;
    if (wts_val != wts)
      NclFree(wts_val);
  }
  else {
    tmp_wts = coerce_input_double(wts,type_wts,npts,0,NULL,NULL);
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
  }
      
/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option) and coerce if necessary.
 */
  smth = (void *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, 
                                 &type_smth, DONT_CARE);
  tmp_smth = coerce_input_double(smth,type_smth,1,0,NULL,NULL);

  if(tmp_smth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing smth array to double precision");
    return(NhlFATAL);
  }
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8, 9, 10 (output x/y/z coordinates).
 */
  xo = (void *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL,
                               &type_yo, DONT_CARE);
  zo = (void *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL,
                               &type_zo, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
 
  nxyz = nxo * nyo * nzo;

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);
  tmp_zo = coerce_input_double(zo,type_zo,nzo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL || tmp_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3x: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxyz;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_ui == NCL_double ||
     type_xo == NCL_double || type_yo == NCL_double ||
     type_zo == NCL_double) {
    type_uo = NCL_double;
    uo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_uo = NCL_float;
    uo      = (void *) calloc(size_output, sizeof(float));
  }
  ndims_uo  = ndims_ui + 2;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3x: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ui != NCL_double) {
/*
 * Coerce npts subsection of ui (tmp_ui) to double.
 */
      coerce_subset_input_double(ui,tmp_ui,index_in,type_ui,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ui to appropriate location in ui.
 */
      tmp_ui = &((double*)ui)[index_in];
    }
/*
 * Call C function.
 */
    tmp_uo = c_csa3xd(inpts, tmp_xi, tmp_yi, tmp_zi, 
                      tmp_ui, tmp_wts, knots, *tmp_smth, nderiv,
                      inxo, inyo, inzo, tmp_xo, tmp_yo, tmp_zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3xd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_uo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(uo,tmp_uo,type_uo,nxyz,index_uo);
    index_in += npts;
    index_uo += nxyz;
    free(tmp_uo);
  }

  if(type_xi   != NCL_double)               NclFree(tmp_xi);
  if(type_yi   != NCL_double)               NclFree(tmp_yi);
  if(type_zi   != NCL_double)               NclFree(tmp_zi);
  if(type_ui   != NCL_double)               NclFree(tmp_ui);
  if(type_wts  != NCL_double || scalar_wts) NclFree(tmp_wts);
  if(type_smth != NCL_double)               NclFree(tmp_smth);
  if(type_xo   != NCL_double)               NclFree(tmp_xo);
  if(type_yo   != NCL_double)               NclFree(tmp_yo);
  if(type_zo   != NCL_double)               NclFree(tmp_zo);
  
  ret = NclReturnValue( uo,  ndims_uo, dsizes_uo, NULL, type_uo, 0);
  NclFree(dsizes_uo);
  return(ret);

}


NhlErrorTypes csa3_W(void)
{
  void *xi, *yi, *zi, *ui, *xo, *yo, *zo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_zi[1];
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  ng_size_t dsizes_xo[1], dsizes_yo[1], dsizes_zo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui;
  NclBasicDataTypes type_xo, type_yo, type_zo;
  double *tmp_xi, *tmp_yi, *tmp_zi, *tmp_xo, *tmp_yo, *tmp_zo;
  double *tmp_ui = NULL;

/*
 * Various. 
 */
  ng_size_t i, npts, nxo, nyo, nzo, nxyz;
  int inpts, inxo, inyo, inzo;
  ng_size_t size_output, size_leftmost;
  int ret, ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

/*
 * Output variables.
 */
  void *uo;
  double *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  NclBasicDataTypes type_uo;

/*
 * Retrieve arguments #0, 1, 2 (x/y/z coordinates).
 */
  xi = (double *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                 &type_xi, DONT_CARE);
  yi = (double *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                 &type_yi, DONT_CARE);
  zi = (double *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                 &type_zi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);
  tmp_zi = coerce_input_double(zi,type_zi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL || tmp_zi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3: Unable to allocate memory for coercing xi/yi/zi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                 &type_ui, DONT_CARE);
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Create temporary ui array.
 */ 
  if(type_ui != NCL_double) {
    tmp_ui = (double*)calloc(npts,sizeof(double));
    if(tmp_ui == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3: Unable to allocate memory for coercing ui array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, 
                                 &type_xo, DONT_CARE);
  yo = (double *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL,
                                 &type_yo, DONT_CARE);
  zo = (double *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL,
                                 &type_zo, DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if( (dsizes_xo[0] > INT_MAX) || (dsizes_yo[0] > INT_MAX) || 
      (dsizes_zo[0] > INT_MAX) ){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3: the length of xo, yo, and/or zo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  nzo  = dsizes_zo[0];
  inxo = (int) nxo;
  inyo = (int) nyo;
  inzo = (int) nzo;
 
  nxyz = nxo * nyo * nzo;
 
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nyo,0,NULL,NULL);
  tmp_zo = coerce_input_double(zo,type_zo,nzo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL || tmp_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxyz;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_ui == NCL_double ||
     type_xo == NCL_double || type_yo == NCL_double ||
     type_zo == NCL_double) {
    type_uo = NCL_double;
    uo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_uo = NCL_float;
    uo      = (void *) calloc(size_output, sizeof(float));
  }
  ndims_uo  = ndims_ui + 2;
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_ui[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ui != NCL_double) {
/*
 * Coerce npts subsection of ui (tmp_ui) to double.
 */
      coerce_subset_input_double(ui,tmp_ui,index_in,type_ui,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ui to appropriate location in ui.
 */
      tmp_ui = &((double*)ui)[index_in];
    }
/*
 * Call C function.
 */
    tmp_uo = c_csa3d(inpts, tmp_xi, tmp_yi, tmp_zi, tmp_ui, knots, 
                     inxo, inyo, inzo, tmp_xo, tmp_yo, tmp_zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3d: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_uo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(uo,tmp_uo,type_uo,nxyz,index_uo);

    index_in += npts;
    index_uo += nxyz;
    free(tmp_uo);
  }
  if(type_xi != NCL_double) NclFree(tmp_xi);
  if(type_yi != NCL_double) NclFree(tmp_yi);
  if(type_zi != NCL_double) NclFree(tmp_zi);
  if(type_ui != NCL_double) NclFree(tmp_ui);
  if(type_xo != NCL_double) NclFree(tmp_xo);
  if(type_yo != NCL_double) NclFree(tmp_yo);
  if(type_zo != NCL_double) NclFree(tmp_zo);
  
  ret = NclReturnValue( uo,  ndims_uo, dsizes_uo, NULL, type_uo, 0);
  NclFree(dsizes_uo);
  return(ret);
}


NhlErrorTypes csa3lx_W(void)
{
  void *xi, *yi, *zi, *ui, *wts, *smth, *xo, *yo, *zo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_zi[1];
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_wts[1];
  int *knots, *nderiv;
  ng_size_t dsizes_xo[1], dsizes_yo[1], dsizes_zo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui, type_wts, type_smth;
  NclBasicDataTypes type_xo, type_yo, type_zo;
  double *tmp_xi, *tmp_yi, *tmp_zi, *wts_val, *tmp_wts, *tmp_smth;
  double *tmp_xo, *tmp_yo, *tmp_zo;
  double *tmp_ui = NULL;

/*
 * Various
 */
  ng_size_t i, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  int ret;
  int scalar_uo, scalar_wts;
  int ier = 0;
  ng_size_t index_in = 0, index_uo = 0;

/*
 * Output variables.
 */
  void *uo;
  double *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  NclBasicDataTypes type_uo;

/*
 * Retrieve arguments #0, 1, 2 (x/y/z coordinates).
 */
  xi = (void *) NclGetArgValue(0, 11, NULL, dsizes_xi, NULL, NULL, 
                               &type_xi, DONT_CARE);
  yi = (void *) NclGetArgValue(1, 11, NULL, dsizes_yi, NULL, NULL, 
                               &type_yi, DONT_CARE);
  zi = (void *) NclGetArgValue(2, 11, NULL, dsizes_zi, NULL, NULL, 
                               &type_zi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lx: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);
  tmp_zi = coerce_input_double(zi,type_zi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL || tmp_zi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing xi/yi/zi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                               &type_ui, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lx: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Create temporary ui array.
 */ 
  if(type_ui != NCL_double) {
    tmp_ui = (double*)calloc(npts,sizeof(double));
    if(tmp_ui == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing ui array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (double *) NclGetArgValue(4, 11, NULL, dsizes_wts, NULL, NULL, 
                                  &type_wts, DONT_CARE);
 
/*
 * wts can be a scalar or one-dimensional. If it is a scalar, 
 * then we need to construct an npts-sized wts array that 
 * is filled with the scalar value.
 */
  scalar_wts = is_scalar(1,dsizes_wts);

/*
 * Check number of dimensions for wts.
 */
  if(!scalar_wts && dsizes_wts[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3x: Argument 4 must be the same length as argument 0 (if it is not a scalar)");
  }

/*
 * If scalar, then first coerce the scalar to double precision.  Then,
 * copy it to an npts array. 
 */
  if(scalar_wts) {
    wts_val = coerce_input_double(wts,type_wts,1,0,NULL,NULL);
    tmp_wts = (double*)calloc(npts,sizeof(double));
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
    for(i = 0; i < npts; i++ ) tmp_wts[i] = *wts_val;
    if (wts_val != wts) 
      NclFree(wts_val);
  }
  else {
    tmp_wts = coerce_input_double(wts,type_wts,npts,0,NULL,NULL);
    if( tmp_wts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing wts to double precision");
      return(NhlFATAL);
    }
  }
      
/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (void *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, 
                                 &type_smth, DONT_CARE);
  tmp_smth = coerce_input_double(smth,type_smth,1,0,NULL,NULL);

  if(tmp_smth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing smth array to double precision");
    return(NhlFATAL);
  }
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (void *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, 
                               &type_xo, DONT_CARE);
  yo = (void *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL,
                               &type_yo, DONT_CARE);
  zo = (void *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL,
                               &type_zo, DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Check sizes of arguments 8, 9, and 10.
 */
  if(nxo != dsizes_yo[0] || nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lx: Arguments #8, 9, and 10 must be the same size.");
    return(NhlFATAL);
  }

  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nxo,0,NULL,NULL);
  tmp_zo = coerce_input_double(zo,type_zo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL || tmp_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3lx: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  scalar_uo = is_scalar(1,dsizes_xo);

  size_output = size_leftmost * nxo;
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_ui == NCL_double ||
     type_xo == NCL_double || type_yo == NCL_double ||
     type_zo == NCL_double) {
    type_uo = NCL_double;
    uo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_uo = NCL_float;
    uo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3lx: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ui != NCL_double) {
/*
 * Coerce npts subsection of ui (tmp_ui) to double.
 */
      coerce_subset_input_double(ui,tmp_ui,index_in,type_ui,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ui to appropriate location in ui.
 */
      tmp_ui = &((double*)ui)[index_in];
    }
/*
 * Call C function.
 */
    tmp_uo = c_csa3lxd(inpts, tmp_xi, tmp_yi, tmp_zi, tmp_ui, tmp_wts, knots, 
                       *tmp_smth, nderiv, inxo, tmp_xo, tmp_yo, tmp_zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3lxd: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_uo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(uo,tmp_uo,type_uo,nxo,index_uo);

    index_in += npts;
    index_uo += nxo;
    free(tmp_uo);
  }

  if(type_xi   != NCL_double)               NclFree(tmp_xi);
  if(type_yi   != NCL_double)               NclFree(tmp_yi);
  if(type_zi   != NCL_double)               NclFree(tmp_zi);
  if(type_ui   != NCL_double)               NclFree(tmp_ui);
  if(type_wts  != NCL_double || scalar_wts) NclFree(tmp_wts);
  if(type_smth != NCL_double)               NclFree(tmp_smth);
  if(type_xo   != NCL_double)               NclFree(tmp_xo);
  if(type_yo   != NCL_double)               NclFree(tmp_yo);
  if(type_zo   != NCL_double)               NclFree(tmp_zo);
  
  ret = NclReturnValue( uo,  ndims_uo, dsizes_uo, NULL, type_uo, 0);
  NclFree(dsizes_uo);
  return(ret);
}


NhlErrorTypes csa3l_W(void)
{
  void *xi, *yi, *zi, *ui, *xo, *yo, *zo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_zi[1];
  int ndims_ui;
  ng_size_t dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  ng_size_t dsizes_xo[1], dsizes_yo[1], dsizes_zo[1];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_ui;
  NclBasicDataTypes type_xo, type_yo, type_zo;
  double *tmp_xi, *tmp_yi, *tmp_zi;
  double *tmp_ui = NULL;
  double *tmp_xo, *tmp_yo, *tmp_zo;

/*
 * Various. 
 */
  ng_size_t i, npts, nxo, size_output, size_leftmost;
  int inpts, inxo;
  ng_size_t scalar_uo;
  int ier = 0;
  ng_size_t index_in = 0, index_uo = 0, ret;

/*
 * Output variables.
 */
  void *uo;
  double *tmp_uo;
  int ndims_uo;
  ng_size_t *dsizes_uo;
  NclBasicDataTypes type_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (double *) NclGetArgValue(0, 8, NULL, dsizes_xi, NULL, NULL, 
                                 &type_xi, DONT_CARE);
  yi = (double *) NclGetArgValue(1, 8, NULL, dsizes_yi, NULL, NULL, 
                                 &type_yi, DONT_CARE);
  zi = (double *) NclGetArgValue(2, 8, NULL, dsizes_zi, NULL, NULL, 
                                 &type_zi, DONT_CARE);
 
/*
 * Check number of dimensions for arguments #0-2.
 */
  if(dsizes_xi[0] != dsizes_yi[0] || dsizes_xi[0] != dsizes_zi[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3l: Arguments #0, 1, and 2 must be the same length");
    return(NhlFATAL);
  }

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3l: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Coerce if ncessary. 
 */
  tmp_xi = coerce_input_double(xi,type_xi,npts,0,NULL,NULL);
  tmp_yi = coerce_input_double(yi,type_yi,npts,0,NULL,NULL);
  tmp_zi = coerce_input_double(zi,type_zi,npts,0,NULL,NULL);

  if(tmp_xi == NULL || tmp_yi == NULL || tmp_zi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3l: Unable to allocate memory for coercing xi/yi/zi arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3 (u values).
 */
  ui = (double *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                 &type_ui, DONT_CARE);
 
/*
 * Check last dimension of argument #3.
 */
  if(dsizes_ui[ndims_ui-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3: The last (rightmost) dimension of argument #3 must be the same length as arguments 0, 1, and 2");
    return(NhlFATAL);
  }

/*
 * Create temporary ui array.
 */ 
  if(type_ui != NCL_double) {
    tmp_ui = (double*)calloc(npts,sizeof(double));
    if(tmp_ui == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3l: Unable to allocate memory for coercing ui array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_ui-1; i++ ) size_leftmost *= dsizes_ui[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, DONT_CARE);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (double *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL,
                                 &type_xo, DONT_CARE);
  yo = (double *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL,
                                 &type_yo, DONT_CARE);
  zo = (double *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL,
                                 &type_zo, DONT_CARE);
/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3l: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;
 
/*
 * Check sizes of arguments 5, 6, and 7.
 */
  if(nxo != dsizes_yo[0] || nxo != dsizes_zo[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3l: Arguments #5, 6, and 7 must be the same size.");
    return(NhlFATAL);
  }
  tmp_xo = coerce_input_double(xo,type_xo,nxo,0,NULL,NULL);
  tmp_yo = coerce_input_double(yo,type_yo,nxo,0,NULL,NULL);
  tmp_zo = coerce_input_double(zo,type_zo,nxo,0,NULL,NULL);

  if(tmp_xo == NULL || tmp_yo == NULL || tmp_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csa3l: Unable to allocate memory for coercing xo/yo arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Calculate space for output array and its dimension sizes.
 */
  size_output = size_leftmost * nxo;
  scalar_uo = is_scalar(1,dsizes_xo);
  if(scalar_uo && ndims_ui > 1) ndims_uo = ndims_ui-1;
  else                          ndims_uo = ndims_ui;
  if(type_xi == NCL_double || type_yi == NCL_double || 
     type_zi == NCL_double || type_ui == NCL_double ||
     type_xo == NCL_double || type_yo == NCL_double ||
     type_zo == NCL_double) {
    type_uo = NCL_double;
    uo      = (void *) calloc(size_output, sizeof(double));
  }
  else {
    type_uo = NCL_float;
    uo      = (void *) calloc(size_output, sizeof(float));
  }
  dsizes_uo = (ng_size_t *) calloc(ndims_uo, sizeof(ng_size_t));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3l: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_ui-1; i++ ) dsizes_uo[i] = dsizes_ui[i];
  if(!scalar_uo || (scalar_uo && ndims_ui == 1)) dsizes_uo[ndims_ui-1] = nxo;

/*
 * Loop through leftmost dimensions.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_ui != NCL_double) {
/*
 * Coerce npts subsection of ui (tmp_ui) to double.
 */
      coerce_subset_input_double(ui,tmp_ui,index_in,type_ui,npts,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_ui to appropriate location in ui.
 */
      tmp_ui = &((double*)ui)[index_in];
    }
/*
 * Call C function.
 */
    tmp_uo = c_csa3ld(inpts, tmp_xi, tmp_yi, tmp_zi, tmp_ui, knots, 
                      inxo, tmp_xo, tmp_yo, tmp_zo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa3ld: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(tmp_uo);
      return(NhlFATAL);
    }
    coerce_output_float_or_double(uo,tmp_uo,type_uo,nxo,index_uo);

    index_in += npts;
    index_uo += nxo;
    free(tmp_uo);
  }
  if(type_xi != NCL_double)  NclFree(tmp_xi);
  if(type_yi != NCL_double)  NclFree(tmp_yi);
  if(type_zi != NCL_double)  NclFree(tmp_zi);
  if(type_ui != NCL_double)  NclFree(tmp_ui);
  if(type_xo != NCL_double)  NclFree(tmp_xo);
  if(type_yo != NCL_double)  NclFree(tmp_yo);
  if(type_zo != NCL_double)  NclFree(tmp_zo);
  
  ret = NclReturnValue( uo,  ndims_uo, dsizes_uo, NULL, type_uo, 0);
  NclFree(dsizes_uo);
  return(ret);
}


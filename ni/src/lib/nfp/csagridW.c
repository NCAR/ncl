#include <string.h>
#include <stdio.h>

#include "wrapper.h"

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include
 */
#include <ncarg/ngmath.h>

char csamsg[61];

NhlErrorTypes csa1xs_W(void)
{
  int i, j, npts, nxo, scalar_wts, size_output, size_leftmost;
  int ier = 0, index_in = 0, index_wts = 0, index_yo = 0;
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];

  float *yo, *yo_tmp;
  int *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 7, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 7, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(ndims_xi != ndims_yi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1xs: Arguments #0 and 1 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xs: Arguments #0 and 1 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Retrieve argument #2 (weights).
 */
  wts = (float *) NclGetArgValue(2, 7, &ndims_wts, dsizes_wts, NULL, 
                                 NULL, NULL, 2);
 
  scalar_wts = is_scalar(ndims_wts,dsizes_wts);

/*
 * Check number of dimensions for arguments #2.
 */
  if(!scalar_wts) {
    if(ndims_xi != ndims_wts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1xs: Argument 2 must have the same number of dimensions as argument 0 (if it is not a scalar)");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_xi; i++ ) {
      if(dsizes_xi[i] != dsizes_wts[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "csa1xs: Argument 2 must have the same dimensions sizes as argument 0 (if it is not a scalar)");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 7, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #4 (smoothing option).
 */
  smth = (float *) NclGetArgValue(4, 7, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #5 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(5, 7, NULL, NULL, NULL, 
                                  NULL, NULL, 2);
/*
 * Retrieve argument #6 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(6, 7, NULL, dsizes_xo, NULL, NULL, NULL, 2);
 
  nxo = dsizes_xo[0];

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (float *) calloc(size_output, sizeof(float));
  dsizes_yo =   (int *) calloc(   ndims_xi, sizeof(int));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_yo[i] = dsizes_xi[i];
  dsizes_yo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1xs(npts, &xi[index_in], &yi[index_in], &wts[index_wts], 
                      *knots, *smth, *nderiv, nxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1xs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    index_in += npts;
    index_yo += nxo;
    if(!scalar_wts) index_wts += npts;
    free(yo_tmp);
  }
  return(NclReturnValue( (void *) yo, ndims_xi, dsizes_yo, NULL, NCL_float, 0));
}

NhlErrorTypes csa1s_W(void)
{
  int i, j, npts, nxo, size_output, size_leftmost;
  int ier = 0, index_in = 0, index_yo = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];

  float *yo, *yo_tmp;
  int *dsizes_yo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 4, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 4, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0 and 1.
 */
  if(ndims_xi != ndims_yi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa1s: Arguments #0 and 1 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa1s: Arguments #0 and 1 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #2 (knots).
 */
  knots = (int *) NclGetArgValue(2, 4, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #3 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(3, 4, NULL, dsizes_xo, NULL, NULL, NULL, 2);
 
  nxo = dsizes_xo[0];

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo;
  yo        = (float *) calloc(size_output, sizeof(float));
  dsizes_yo =   (int *) calloc(   ndims_xi, sizeof(int));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa1s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_yo[i] = dsizes_xi[i];
  dsizes_yo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    yo_tmp = c_csa1s(npts, &xi[index_in], &yi[index_in], *knots, nxo, xo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa1s: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(yo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo; j++) {
      yo[index_yo+j] = yo_tmp[j];
    }
    index_in += npts;
    index_yo += nxo;
    free(yo_tmp);
  }
  return(NclReturnValue( (void *) yo, ndims_xi, dsizes_yo, NULL, NCL_float, 0));

}

NhlErrorTypes csa2s_W(void)
{
  int i, j, npts, nxo, nyo, size_output, size_leftmost;
  int ier = 0, index_in = 0, index_zo = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo, *zo_tmp;
  int ndims_zo, *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, and 2.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2s: Arguments #0, 1, and 2 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2s: Arguments #0, 1, and 2 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];

/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, 2);
  nyo = dsizes_yo[0];
 
/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo * nyo;
  zo        = (float *) calloc(size_output, sizeof(float));

  ndims_zo  = ndims_xi + 1;
  dsizes_zo =   (int *) calloc(ndims_zo, sizeof(int));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_xi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2s(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                     knots, nxo, nyo, xo, yo, &ier);

    if (ier != 0) {
      sprintf(csamsg, "c_csa2s: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo*nyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nyo*nxo;
    free(zo_tmp);
  }
  
  return(NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_float, 0));
}

NhlErrorTypes csa2xs_W(void)
{
  int i, j, npts, nxo, nyo, size_output, size_leftmost, scalar_wts;
  int ier = 0, index_in = 0, index_zo = 0, index_wts = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo, *zo_tmp;
  int ndims_zo, *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, and 2.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2xs: Arguments #0, 1, and 2 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xs: Arguments #0, 1, and 2 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, &ndims_wts, dsizes_wts, NULL, NULL, 
                                 NULL, 2);
 
  scalar_wts = is_scalar(ndims_wts,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(ndims_xi != ndims_wts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2xs: Argument 3 must have the same number of dimensions as argument 0 (if it is not a scalar)");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_xi; i++ ) {
      if(dsizes_xi[i] != dsizes_wts[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "csa2xs: Argument 3 must have the same dimensions sizes as argument 0 (if it is not a scalar)");
        return(NhlFATAL);
      }
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, 2);
  nyo = dsizes_yo[0];

/*
 * Calculate space for output array and its dimension sizes.
 */

  size_output = size_leftmost * nxo * nyo;
  zo        = (float *) calloc(size_output, sizeof(float));

  ndims_zo  = ndims_xi + 1;
  dsizes_zo =   (int *) calloc(ndims_zo, sizeof(int));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_zo-2; i++ ) dsizes_zo[i] = dsizes_xi[i];
  dsizes_zo[ndims_zo-2] = nxo;
  dsizes_zo[ndims_zo-1] = nyo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2xs(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                      &wts[index_wts], knots, *smth, nderiv,
                      nxo, nyo, xo, yo, &ier);
    if (ier != 0) {
      sprintf(csamsg, "c_csa2xs: Error number %d.", ier);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csamsg);
      free(zo_tmp);
      return(NhlFATAL);
    }
    for (j = 0; j < nxo*nyo; j++) {
      zo[index_zo+j] = zo_tmp[j];
    }
    index_in += npts;
    index_zo += nyo*nxo;
    if(!scalar_wts) index_wts += npts;
    free(zo_tmp);
  }

  return(NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_float, 0));
}

NhlErrorTypes csa2ls_W(void)
{
  int i, j, npts, nxo, size_output, size_leftmost;
  int ier = 0, index_in = 0, index_zo = 0, scalar_zo;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo, *zo_tmp;
  int ndims_zo, *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 6, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);

/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 6, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 6, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, and 2.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2ls: Arguments #0, 1, and 2 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2ls: Arguments #0, 1, and 2 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #3 (knots).
 */
  knots = (int *) NclGetArgValue(3, 6, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #4 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(4, 6, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #5 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(5, 6, NULL, dsizes_yo, NULL, NULL, NULL, 2);
 
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
  if(scalar_zo && ndims_xi > 1) ndims_zo = ndims_xi-1;
  else                          ndims_zo = ndims_xi;
  zo        = (float *) calloc(size_output, sizeof(float));

  dsizes_zo =   (int *) calloc(ndims_zo, sizeof(int));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2ls: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_zo[i] = dsizes_xi[i];
  if(!scalar_zo || (scalar_zo && ndims_xi == 1)) dsizes_zo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2ls(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                      knots, nxo, xo, yo, &ier);
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

  return(NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, NULL, 
                         NCL_float, 0));
}

NhlErrorTypes csa2lxs_W(void)
{
  int i, j, npts, nxo, size_output, size_leftmost;
  int scalar_zo, scalar_wts;
  int ier = 0, index_in = 0, index_zo = 0, index_wts = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];

  float *zo, *zo_tmp;
  int ndims_zo, *dsizes_zo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 9, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 9, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z values).
 */
  zi = (float *) NclGetArgValue(2, 9, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, and 2.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa2lxs: Arguments #0, 1, and 2 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxs: Arguments #0, 1, and 2 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #3 (weights).
 */
  wts = (float *) NclGetArgValue(3, 9, &ndims_wts, dsizes_wts, NULL, NULL, 
                                 NULL, 2);

  scalar_wts = is_scalar(ndims_wts,dsizes_wts);

/*
 * Check number of dimensions for arguments #3.
 */
  if(!scalar_wts) {
    if(ndims_xi != ndims_wts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa2lxs: Argument 3 must have the same number of dimensions as argument 0 (if it is not a scalar)");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_xi; i++ ) {
      if(dsizes_xi[i] != dsizes_wts[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "csa2lxs: Argument 3 must have the same dimensions sizes as argument 0 (if it is not a scalar)");
        return(NhlFATAL);
      }
    }
  }

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #5 (smoothing option).
 */
  smth = (float *) NclGetArgValue(5, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #6 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(6, 9, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #7 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(7, 9, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #8 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(8, 9, NULL, dsizes_yo, NULL, NULL, NULL, 2);
 
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
  if(scalar_zo && ndims_xi > 1) ndims_zo = ndims_xi-1;
  else                          ndims_zo = ndims_xi;
  zo        = (float *) calloc(size_output, sizeof(float));

  dsizes_zo =   (int *) calloc(ndims_zo, sizeof(int));

  if(zo == NULL || dsizes_zo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa2lxs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_zo[i] = dsizes_xi[i];
  if(!scalar_zo || (scalar_zo && ndims_xi == 1)) dsizes_zo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    zo_tmp = c_csa2lxs(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                       &wts[index_wts], knots, *smth, nderiv,
                       nxo, xo, yo, &ier);

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
    if(!scalar_wts) index_wts += npts;
    free(zo_tmp);
  }
  return(NclReturnValue( (void *) zo,  ndims_zo, dsizes_zo, 
                         NULL, NCL_float, 0));
}

NhlErrorTypes csa3xs_W(void)
{
  int i, j, npts, nxo, nyo, nzo, nxyz;
  int size_output, size_leftmost, scalar_wts;
  int ier = 0, index_in = 0, index_uo = 0, index_wts = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo, *uo_tmp;
  int ndims_uo, *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, 2);

/*
 * Check number of dimensions for arguments #0, 1, 2, and 3.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi || ndims_xi != ndims_ui) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3xs: Arguments #0, 1, 2, and 3 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i] ||
       dsizes_xi[i] != dsizes_ui[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3xs: Arguments #0, 1, 2, and 3 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, &ndims_wts, dsizes_wts, NULL, NULL, 
                                 NULL, 2);
 
  scalar_wts = is_scalar(ndims_wts,dsizes_wts);

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, 2);
  nyo = dsizes_yo[0];

/*
 * Retrieve argument #10 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, 2);
  nzo = dsizes_zo[0];
 
/*
 * Calculate space for output array and its dimension sizes.
 */

  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (float *) calloc(size_output, sizeof(float));

  ndims_uo  = ndims_xi + 2;
  dsizes_uo =   (int *) calloc(ndims_uo, sizeof(int));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3xs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_xi[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3xs(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                      &ui[index_in], &wts[index_wts], knots, *smth, nderiv,
                      nxo, nyo, nzo, xo, yo, zo, &ier);
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
    if(!scalar_wts) index_wts += npts;
    free(uo_tmp);
  }
  return(NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_float, 0));
}

NhlErrorTypes csa3s_W(void)
{
  int i, j, npts, nxo, nyo, nzo, nxyz;
  int size_output, size_leftmost;
  int ier = 0, index_in = 0, index_uo = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo, *uo_tmp;
  int ndims_uo, *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);

/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, 2, and 3.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi || ndims_xi != ndims_ui) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3s: Arguments #0, 1, 2, and 3 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i] ||
       dsizes_xi[i] != dsizes_ui[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3s: Arguments #0, 1, 2, and 3 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, 2);
  nyo = dsizes_yo[0];
 
/*
 * Retrieve argument #7 (output z coordinates).
 */
  zo = (float *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, 2);
  nzo = dsizes_zo[0];
 
/*
 * Calculate space for output array and its dimension sizes.
 */
  nxyz = nxo * nyo * nzo;
  size_output = size_leftmost * nxyz;
  uo        = (float *) calloc(size_output, sizeof(float));

  ndims_uo  = ndims_xi + 2;
  dsizes_uo =   (int *) calloc(ndims_uo, sizeof(int));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3s: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_uo-3; i++ ) dsizes_uo[i] = dsizes_xi[i];
  dsizes_uo[ndims_uo-3] = nxo;
  dsizes_uo[ndims_uo-2] = nyo;
  dsizes_uo[ndims_uo-1] = nzo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3s(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                     &ui[index_in], knots, nxo, nyo, nzo, xo, yo, zo, &ier);
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
  return(NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL, 
                         NCL_float, 0));
}

NhlErrorTypes csa3lxs_W(void)
{
  int i, j, npts, nxo, size_output, size_leftmost;
  int scalar_uo, scalar_wts;
  int ier = 0, index_in = 0, index_uo = 0, index_wts = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  float *wts;
  int ndims_wts, dsizes_wts[NCL_MAX_DIMENSIONS];
  int *knots;
  float *smth;
  int *nderiv;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo, *uo_tmp;
  int ndims_uo, *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 11, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 11, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 11, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 11, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, 2, and 3.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi || ndims_xi != ndims_ui) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3lxs: Arguments #0, 1, 2, and 3 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i] ||
       dsizes_xi[i] != dsizes_ui[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3lxs: Arguments #0, 1, 2, and 3 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #4 (weights).
 */
  wts = (float *) NclGetArgValue(4, 11, &ndims_wts, dsizes_wts, NULL, NULL, 
                                 NULL, 2);
 
  scalar_wts = is_scalar(ndims_wts,dsizes_wts);

/*
 * Retrieve argument #5 (knots).
 */
  knots = (int *) NclGetArgValue(5, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #6 (smoothing option).
 */
  smth = (float *) NclGetArgValue(6, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #7 (derivative flag).
 */
  nderiv = (int *) NclGetArgValue(7, 11, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #8 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(8, 11, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #9 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(9, 11, NULL, dsizes_yo, NULL, NULL, NULL, 2);
 
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
  zo = (float *) NclGetArgValue(10, 11, NULL, dsizes_zo, NULL, NULL, NULL, 2);
 
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
  if(scalar_uo && ndims_xi > 1) ndims_uo = ndims_xi-1;
  else                          ndims_uo = ndims_xi;
  uo        = (float *) calloc(size_output, sizeof(float));

  dsizes_uo =   (int *) calloc(ndims_uo, sizeof(int));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3lxs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_uo[i] = dsizes_xi[i];
  if(!scalar_uo || (scalar_uo && ndims_xi == 1)) dsizes_uo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3lxs(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                       &ui[index_in], &wts[index_wts], knots, *smth, nderiv,
                       nxo, xo, yo, zo, &ier);
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
    if(!scalar_wts) index_wts += npts;
    free(uo_tmp);
  }

  return(NclReturnValue( (void *) uo, ndims_uo, dsizes_uo, 
                         NULL, NCL_float, 0));
}

NhlErrorTypes csa3ls_W(void)
{
  int i, j, npts, nxo, size_output, size_leftmost, scalar_uo;
  int ier = 0, index_in = 0, index_uo = 0, index_wts = 0;

  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *ui;
  int ndims_ui, dsizes_ui[NCL_MAX_DIMENSIONS];
  int *knots;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int dsizes_zo[NCL_MAX_DIMENSIONS];

  float *uo, *uo_tmp;
  int ndims_uo, *dsizes_uo;

/*
 * Retrieve argument #0 (x coordinates).
 */
  xi = (float *) NclGetArgValue(0, 8, &ndims_xi, dsizes_xi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #1 (y coordinates).
 */
  yi = (float *) NclGetArgValue(1, 8, &ndims_yi, dsizes_yi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #2 (z coordinates).
 */
  zi = (float *) NclGetArgValue(2, 8, &ndims_zi, dsizes_zi, NULL, NULL, 
                                NULL, 2);
 
/*
 * Retrieve argument #3 (u values).
 */
  ui = (float *) NclGetArgValue(3, 8, &ndims_ui, dsizes_ui, NULL, NULL, 
                                NULL, 2);
 
/*
 * Check number of dimensions for arguments #0, 1, 2, and 3.
 */
  if(ndims_xi != ndims_yi || ndims_xi != ndims_zi || ndims_xi != ndims_ui) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csa3ls: Arguments #0, 1, 2, and 3 must have the same number of dimensions.");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_xi; i++ ) {
    if(dsizes_xi[i] != dsizes_yi[i] || dsizes_xi[i] != dsizes_zi[i] ||
       dsizes_xi[i] != dsizes_ui[i]) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "csa3ls: Arguments #0, 1, 2, and 3 must have the same dimensions sizes.");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  npts = dsizes_xi[ndims_xi-1];
  size_leftmost = 1;
  for( i = 0; i < ndims_xi-1; i++ ) size_leftmost *= dsizes_xi[i];

/*
 * Retrieve argument #4 (knots).
 */
  knots = (int *) NclGetArgValue(4, 8, NULL, NULL, NULL, NULL, NULL, 2);
 
/*
 * Retrieve argument #5 (output x coordinates).
 */
  xo = (float *) NclGetArgValue(5, 8, NULL, dsizes_xo, NULL, NULL, NULL, 2);
  nxo = dsizes_xo[0];
 
/*
 * Retrieve argument #6 (output y coordinates).
 */
  yo = (float *) NclGetArgValue(6, 8, NULL, dsizes_yo, NULL, NULL, NULL, 2);
 
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
  zo = (float *) NclGetArgValue(7, 8, NULL, dsizes_zo, NULL, NULL, NULL, 2);
 
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
  if(scalar_uo && ndims_xi > 1) ndims_uo = ndims_xi-1;
  else                          ndims_uo = ndims_xi;
  uo        = (float *) calloc(size_output, sizeof(float));

  dsizes_uo =   (int *) calloc(ndims_uo, sizeof(int));

  if(uo == NULL || dsizes_uo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "csa3ls: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_xi-1; i++ ) dsizes_uo[i] = dsizes_xi[i];
  if(!scalar_uo || (scalar_uo && ndims_xi == 1)) dsizes_uo[ndims_xi-1] = nxo;

/*
 *  Call the C procedure.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    uo_tmp = c_csa3ls(npts, &xi[index_in], &yi[index_in], &zi[index_in], 
                      &ui[index_in], knots, nxo, xo, yo, zo, &ier);
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
  return(NclReturnValue( (void *) uo,  ndims_uo, dsizes_uo, NULL,
                         NCL_float, 0));
}

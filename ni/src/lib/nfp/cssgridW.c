#include <stdio.h>
#include <string.h>

/*
 * The following are required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

/*
 *  Declare the required C external functions.
 */
extern int   *c_csstri(int, float [], float [], float [], int *, int *);
extern float *c_cssgrid(int, float [], float [], float [], float [],
                        int, int, float [], float [], int *);
extern void   c_cstrans(int, float *, float *, float *, float *, float *);
extern void   c_csscoord(float, float, float, float *, float *, float *);
extern void   c_csvoro(int, float [], float [], float [], int, int,
                       float [], float [], float [], float [], int *,
                       int *, int [], int *);


int  cserr;
char csmsg[61];

NhlErrorTypes csvoro_W(void)
{

/*
 *  Input variables
 */

  float     *x;
  int       ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  float     *y;
  int       ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  float     *z;
  int       ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
  int       *ni;
  int       ndims_ni, dsizes_ni[NCL_MAX_DIMENSIONS];
  int       *nf;
  int       ndims_nf, dsizes_nf[NCL_MAX_DIMENSIONS];

  int       has_missing_x, has_missing_y, has_missing_z;
  int       has_missing_ni, has_missing_nf;
  NclScalar missing_x, missing_y, missing_z;
  NclScalar missing_ni, missing_nf;

/*
 *  Output variables
 */
  int       *nca;
  int       ndims_nca, dsizes_nca[NCL_MAX_DIMENSIONS];
  int       *numv;
  int       ndims_numv, dsizes_numv[NCL_MAX_DIMENSIONS];
  float     *xc;
  int       ndims_xc, dsizes_xc[NCL_MAX_DIMENSIONS];
  float     *yc;
  int       ndims_yc, dsizes_yc[NCL_MAX_DIMENSIONS];
  float     *zc;
  int       ndims_zc, dsizes_zc[NCL_MAX_DIMENSIONS];
  float     *rc;
  int       ndims_rc, dsizes_rc[NCL_MAX_DIMENSIONS];
  int       *nv;
  int       ndims_nv, dsizes_nv[NCL_MAX_DIMENSIONS];

/*
 *  Local variables.
 */
  int       i, num_points;
  float     pnm;

/*
 *  Retrieve argument #1 (input)
 */
  x = (float *) NclGetArgValue(
      0,
      12,
      &ndims_x,
      dsizes_x,
      &missing_x,
      &has_missing_x,
      NULL,
      2);

/*
 *  Retrieve argument #2 (input)
 */
  y = (float *) NclGetArgValue(
      1,
      12,
      &ndims_y,
      dsizes_y,
      &missing_y,
      &has_missing_y,
      NULL,
      2);

/*
 *  Retrieve argument #3 (input)
 */
  z = (float *) NclGetArgValue(
      2,
      12,
      &ndims_z,
      dsizes_z,
      &missing_z,
      &has_missing_z,
      NULL,
      2);

/*
 *  Retrieve argument #4 (input)
 */
  ni = (int *) NclGetArgValue(
       3,
       12,
       &ndims_ni,
       dsizes_ni,
       &missing_ni,
       &has_missing_ni,
       NULL,
       2);

/*
 *  Retrieve argument #5 (input)
 */
  nf = (int *) NclGetArgValue(
       4,
       12,
       &ndims_nf,
       dsizes_nf,
       &missing_nf,
       &has_missing_nf,
       NULL,
       2);

/*
 *  Retrieve argument #6 (output)
 */
  xc = (float *) NclGetArgValue(
       5,
       12,
       &ndims_xc,
       dsizes_xc,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Retrieve argument #7 (output)
 */
  yc = (float *) NclGetArgValue(
       6,
       12,
       &ndims_yc,
       dsizes_yc,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Retrieve argument #8 (output)
 */
  zc = (float *) NclGetArgValue(
       7,
       12,
       &ndims_zc,
       dsizes_zc,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Retrieve argument #9 (output)
 */
  rc = (float *) NclGetArgValue(
       8,
       12,
       &ndims_rc,
       dsizes_rc,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Retrieve argument #10 (output)
 */
  nca = (int *) NclGetArgValue(
        9,
        12,
        &ndims_nca,
        dsizes_nca,
        NULL,
        NULL,
        NULL,
        1);

/*
 *  Retrieve argument #11 (output)
 */
  numv = (int *) NclGetArgValue(
         10,
         12,
         &ndims_numv,
         dsizes_numv,
         NULL,
         NULL,
         NULL,
         1);

/*
 *  Retrieve argument #12 (output)
 */
  nv = (int *) NclGetArgValue(
       11,
       12,
       &ndims_nv,
       dsizes_nv,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Check that all of the arguments for x, y, and z are the same size.
 */
  if ( (dsizes_x[0] != dsizes_y[0]) || 
       (dsizes_y[0] != dsizes_z[0])) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csvoro: all arrays must be of the same size");
         return(NhlFATAL);
  }
  else {
        num_points = dsizes_x[0];
  }

/*
 *  Check that the output arrays for the circumcircles are big enough.
 */
  if (dsizes_xc[0] < num_points || dsizes_yc[0] < num_points ||
      dsizes_zc[0] < num_points || dsizes_rc[0] < num_points) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csvoro: arrays for circumcircle centers too small");
  }

/*
 *  Check that the array for the vertex indices is big enough.
 */
  if (dsizes_nv[0] < num_points) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csvoro: array for vertices too small");
  }

/*
 *  Check for missing values.
 */
  if (has_missing_x) {
    for (i = 0; i < num_points; i++) {
      if (x[i] == missing_x.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csvoro: missing values not allowed in input x.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_y) {
    for (i = 0; i < num_points; i++) {
      if (y[i] == missing_y.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csvoro: missing values not allowed in input y.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_z) {
    for (i = 0; i < num_points; i++) {
      if (z[i] == missing_z.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csvoro: missing values not allowed in input z.");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Make the call to c_csvoro.
 */
    c_csvoro(num_points, x, y, z, *ni, *nf, xc, yc, zc, rc, nca, numv,
             nv, &cserr);
    if (cserr != 0) {
      sprintf(csmsg, "csvoro: Error number %d.", cserr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
      return(NhlFATAL);
    }
    else {
      return(NhlNOERROR);
    }
}
 
NhlErrorTypes csscoord_W(void)
{

/*
 *  Input array variables
 */

  float     *x;
  int       ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  float     *y;
  int       ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  float     *z;
  int       ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];

  int       has_missing_x, has_missing_y, has_missing_z;
  NclScalar missing_x, missing_y, missing_z;

/*
 *  Output array variables
 */
  float     *latitudes;
  int       ndims_latitudes, dsizes_latitudes[NCL_MAX_DIMENSIONS];
  float     *longitudes;
  int       ndims_longitudes, dsizes_longitudes[NCL_MAX_DIMENSIONS];

/*
 *  Local variables.
 */
  int       i, num_points;
  float     pnm;

/*
 *  Retrieve argument #1 (input)
 */
  x = (float *) NclGetArgValue(
      0,
      5,
      &ndims_x,
      dsizes_x,
      &missing_x,
      &has_missing_x,
      NULL,
      2);

/*
 *  Retrieve argument #2 (input)
 */
  y = (float *) NclGetArgValue(
      1,
      5,
      &ndims_y,
      dsizes_y,
      &missing_y,
      &has_missing_y,
      NULL,
      2);

/*
 *  Retrieve argument #3 (input)
 */
  z = (float *) NclGetArgValue(
      2,
      5,
      &ndims_z,
      dsizes_z,
      &missing_z,
      &has_missing_z,
      NULL,
      2);

/*
 *  Retrieve argument #4 (output)
 */
  latitudes = (float *) NclGetArgValue(
              3,
              5,
              &ndims_latitudes,
              dsizes_latitudes,
              NULL,
              NULL,
              NULL,
              1);

/*
 *  Retrieve argument #5 (output)
 */
  longitudes = (float *) NclGetArgValue(
               4,
               5,
               &ndims_longitudes,
               dsizes_longitudes,
               NULL,
               NULL,
               NULL,
               1);

/*
 *  Check that all of the arguments are of the same size.
 */
  if ( (dsizes_x[0] != dsizes_y[0]) || 
       (dsizes_y[0] != dsizes_z[0]) ||
       (dsizes_z[0] != dsizes_latitudes[0]) || 
       (dsizes_latitudes[0] != dsizes_longitudes[0]) ) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csscoord: all arrays must be of the same size");
         return(NhlFATAL);
  }
  else {
        num_points = dsizes_x[0];
  }

/*
 *  Check for missing values.
 */
  if (has_missing_x) {
    for (i = 0; i < num_points; i++) {
      if (x[i] == missing_x.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csscoord: missing values not allowed in input x.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_y) {
    for (i = 0; i < num_points; i++) {
      if (y[i] == missing_y.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csscoord: missing values not allowed in input y.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_z) {
    for (i = 0; i < num_points; i++) {
      if (z[i] == missing_z.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csscoord: missing values not allowed in input z.");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Make the calls to c_csscoord.
 */
  for (i = 0; i < num_points; i++) {
    c_csscoord(x[i], y[i], z[i], latitudes+i, longitudes+i, &pnm);
  }
  return(NhlNOERROR);
}

NhlErrorTypes cstrans_W(void)
{

/*
 *  Input array variables
 */

  float     *latitudes;
  int       ndims_latitudes, dsizes_latitudes[NCL_MAX_DIMENSIONS];
  float     *longitudes;
  int       ndims_longitudes, dsizes_longitudes[NCL_MAX_DIMENSIONS];

  int       has_missing_latitudes, has_missing_longitudes;
  NclScalar missing_latitudes, missing_longitudes;

/*
 *  Output array variables
 */
  float     *x;
  int       ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  float     *y;
  int       ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  float     *z;
  int       ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];

/*
 *  Local variables.
 */
  int       i, num_points;

/*
 *  Retrieve argument #1 (input)
 */
  latitudes = (float *) NclGetArgValue(
              0,
              5,
              &ndims_latitudes,
              dsizes_latitudes,
              &missing_latitudes,
              &has_missing_latitudes,
              NULL,
              2);

/*
 *  Retrieve argument #2 (input)
 */
  longitudes = (float *) NclGetArgValue(
               1,
               5,
               &ndims_longitudes,
               dsizes_longitudes,
               &missing_longitudes,
               &has_missing_longitudes,
               NULL,
               2);

/*
 *  Retrieve argument #3 (output)
 */
  x = (float *) NclGetArgValue(
      2,
      5,
      &ndims_x,
      dsizes_x,
      NULL,
      NULL,
      NULL,
      1);

/*
 *  Retrieve argument #4 (output)
 */
  y = (float *) NclGetArgValue(
      3,
      5,
      &ndims_y,
      dsizes_y,
      NULL,
      NULL,
      NULL,
      1);

/*
 *  Retrieve argument #5 (output)
 */
  z = (float *) NclGetArgValue(
      4,
      5,
      &ndims_z,
      dsizes_z,
      NULL,
      NULL,
      NULL,
      1);

/*
 *  Check that all of the arguments are of the same size.
 */
  if ( (dsizes_latitudes[0] != dsizes_longitudes[0]) || 
       (dsizes_longitudes[0] != dsizes_x[0]) ||
       (dsizes_x[0] != dsizes_y[0]) || 
       (dsizes_y[0] != dsizes_z[0]) ) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "cstrans: all arrays must be of the same size");
         return(NhlFATAL);
  }
  else {
        num_points = dsizes_latitudes[0];
  }

/*
 *  Check for missing values.
 */
  if (has_missing_latitudes) {
    for (i = 0; i < num_points; i++) {
      if (latitudes[i] == missing_latitudes.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cstrans: missing values not allowed in input latitudes.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_longitudes) {
    for (i = 0; i < num_points; i++) {
      if (longitudes[i] == missing_longitudes.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cstrans: missing values not allowed in input longitudes.");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Make the call to c_cstrans.
 */
  c_cstrans(num_points, latitudes, longitudes, x, y, z);
  return(NhlNOERROR);

}

NhlErrorTypes csstri_W(void)
{

/*
 *  Input array variables
 */

  float      *x;
  int        ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  float      *y;
  int        ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  float      *z;
  int        ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
 
  int        has_missing_x, has_missing_y, has_missing_z;
  NclScalar  missing_x, missing_y, missing_z;

  int        *trlist, nt, dsizes_trlist[2], i, j, num_points, num_missing;

/*
 *  Retrieve argument #1 (input)
 */
  x = (float *) NclGetArgValue(
      0,
      3,
      &ndims_x,
      dsizes_x,
      &missing_x,
      &has_missing_x,
      NULL,
      2);

/*
 *  Retrieve argument #2 (input)
 */
  y = (float *) NclGetArgValue(
      1,
      3,
      &ndims_y,
      dsizes_y,
      &missing_y,
      &has_missing_y,
      NULL,
      2);

/*
 *  Retrieve argument #3 (output)
 */
  z = (float *) NclGetArgValue(
      2,
      3,
      &ndims_z,
      dsizes_z,
      &missing_z,
      &has_missing_z,
      NULL,
      2);

/*
 *  Check that all of the arguments are of the same size.
 */
  if( (dsizes_x[0] != dsizes_y[0]) || (dsizes_y[0] != dsizes_z[0]) ) {
       NhlPError(NhlFATAL,NhlEUNKNOWN,
         "csstri: all arrays must be of the same size");
       return(NhlFATAL);
  }
  else {
    num_points = dsizes_x[0];
    num_missing = 0;
  }

/*
 *  Check for missing values.  Each argument x, y, and z must 
 *  be checked separately, since it may be that _FillValue may be
 *  set for just one or two of the arguments.
 *
 *  If a missing value is found, ignore that input point by collapsing
 *  all of the input arrays.
 */
  if (has_missing_x) {
    i = 0;
    lab0:
    if ((x[i] == missing_x.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csstri: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab0;
    }
    i++;
    if (i < num_points) goto lab0;
  }
  if (has_missing_y) {
    i = 0;
    lab1:
    if ((y[i] == missing_y.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csstri: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab1;
    }
    i++;
    if (i < num_points) goto lab1;
  }
  if (has_missing_z) {
    i = 0;
    lab2:
    if ((z[i] == missing_z.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "csstri: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab2;
    }
    i++;
    if (i < num_points) goto lab2;
  }

/*
 *  Issue warning if missing values have been detected.
 */
  if (num_missing > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,
      "csstri: missing values in %d input points - those points ignored.",
      num_missing);
  }

/*
 *  Make the call to the C function.
 */
  trlist = c_csstri(num_points, x, y, z, &nt, &cserr);
  if (cserr != 0) {
    sprintf(csmsg, "csstri: Error number %d.", cserr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
    return(NhlFATAL);
  }

  dsizes_trlist[0] = nt;
  dsizes_trlist[1] = 3;
  return(NclReturnValue(
                         (void *) trlist,
                         2,
                         dsizes_trlist,
                         NULL,
                         NCL_int,
                         0
                       )
         );
	
}

NhlErrorTypes cssgrid_W(void)
{

/*
 *  Input array variables
 */

  float     *x;
  int       ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  float     *y;
  int       ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  float     *z;
  int       ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
  float     *fval;
  int       ndims_fval, dsizes_fval[NCL_MAX_DIMENSIONS];
  float     *latitudes;
  int       ndims_latitudes, dsizes_latitudes[NCL_MAX_DIMENSIONS];
  float     *longitudes;
  int       ndims_longitudes, dsizes_longitudes[NCL_MAX_DIMENSIONS];

  int       has_missing_x, has_missing_y, has_missing_z, has_missing_fval,
            has_missing_latitudes, has_missing_longitudes;
  NclScalar missing_x, missing_y, missing_z, missing_fval,
            missing_latitudes, missing_longitudes;

  float     *fgrid;
  int       ndims_fgrid, dsizes_fgrid[2];

  int       i, j, num_points, num_missing;

/*
 *  Retrieve argument #1 (input)
 */
  x = (float *) NclGetArgValue(
      0,
      6,
      &ndims_x,
      dsizes_x,
      &missing_x,
      &has_missing_x,
      NULL,
      2);

/*
 *  Retrieve argument #2 (input)
 */
  y = (float *) NclGetArgValue(
      1,
      6,
      &ndims_y,
      dsizes_y,
      &missing_y,
      &has_missing_y,
      NULL,
      2);

/*
 *  Retrieve argument #3 (input)
 */
  z = (float *) NclGetArgValue(
      2,
      6,
      &ndims_z,
      dsizes_z,
      &missing_z,
      &has_missing_z,
      NULL,
      2);

/*
 *  Retrieve argument #4 (input)
 */
  fval = (float *) NclGetArgValue(
         3,
         6,
         &ndims_fval,
         dsizes_fval,
         &missing_fval,
         &has_missing_fval,
         NULL,
         2);

/*
 *  Retrieve argument #5 (input)
 */
  latitudes = (float *) NclGetArgValue(
              4,
              6,
              &ndims_latitudes,
              dsizes_latitudes,
              &missing_latitudes,
              &has_missing_latitudes,
              NULL,
              2);

/*
 *  Retrieve argument #6 (input)
 */
  longitudes = (float *) NclGetArgValue(
               5,
               6,
               &ndims_longitudes,
               dsizes_longitudes,
               &missing_longitudes,
               &has_missing_longitudes,
               NULL,
               2);

/*
 *  Check that all of the first four arguments are of the same size.
 */
  if( (dsizes_x[0] != dsizes_y[0]) || 
      (dsizes_y[0] != dsizes_z[0]) ||
      (dsizes_z[0] != dsizes_fval[0]) ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
           "cssgrid: all arrays for the input functional values" 
           " must be of the same size");
        return(NhlFATAL);
  }
  else {
    num_points = dsizes_x[0]; 
    num_missing = 0;
  }

/*
 *  Check for missing values.  Each argument x, y, z, and fval must 
 *  be checked separately, since it may be that _FillValue may be
 *  set for just one or two of the arguments.
 *
 *  If a missing value is found, ignore that input point by collapsing
 *  all of the input arrays.
 */
  if (has_missing_x) {
    i = 0;
    lab0:
    if ((x[i] == missing_x.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
        fval[j] = fval[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab0;
    }
    i++;
    if (i < num_points) goto lab0;
  }
  if (has_missing_y) {
    i = 0;
    lab1:
    if ((y[i] == missing_y.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
        fval[j] = fval[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab1;
    }
    i++;
    if (i < num_points) goto lab1;
  }
  if (has_missing_z) {
    i = 0;
    lab2:
    if ((z[i] == missing_z.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
        fval[j] = fval[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
        
      }
      num_missing++;
      goto lab2;
    }
    i++;
    if (i < num_points) goto lab2;
  }
  if (has_missing_fval) {
    i = 0;
    lab3:
    if ((fval[i] == missing_fval.floatval) && (i < num_points)) {
      for (j = i; j < num_points - 1; j++) {
        x[j] = x[j+1];
        y[j] = y[j+1];
        z[j] = z[j+1];
        fval[j] = fval[j+1];
      }
      num_points--;
      if (num_points < 3) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: missing values in the input data have reduced the number "
          "of valid points to less than 3.\n");
        return(NhlFATAL);
      }
      num_missing++;
      goto lab3;
    }
    i++;
    if (i < num_points) goto lab3;
  }

/*
 *  Issue warning if missing values have been detected.
 */
  if (num_missing > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,
      "cssgrid: missing values in %d input points - those points ignored.",
      num_missing);
  }

/*
 *  Missing values not allowed in latitude or longitude arrays.
 */
  if (has_missing_latitudes) {
    for (i = 0; i < dsizes_latitudes[0]; i++) {
      if (latitudes[i] == missing_latitudes.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
              "cssgrid: argument (3) not allowed to contain a missing value.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing_longitudes) {
    for (i = 0; i < dsizes_longitudes[0]; i++) {
      if (longitudes[i] == missing_longitudes.floatval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
              "cssgrid: argument (4) not allowed to contain a missing value.");
        return(NhlFATAL);
      }
    }
  }


/*
 *  Make the call to the C function.
 */
  fgrid = c_cssgrid(num_points, x, y, z, fval, 
                    dsizes_latitudes[0], dsizes_longitudes[0],
                    latitudes, longitudes, &cserr);
  if (cserr != 0) {
    sprintf(csmsg, "cssgrid: Error number %d.", cserr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
    return(NhlFATAL);
  }

/*
 * Return value to NCL
 */
  ndims_fgrid = 2;
  dsizes_fgrid[0] = dsizes_latitudes[0];
  dsizes_fgrid[1] = dsizes_longitudes[0];
  return( NclReturnValue(
                         (void*)fgrid,
                         ndims_fgrid,
                         dsizes_fgrid,
                         NULL,
                         NCL_float,
                         0)
        );
}

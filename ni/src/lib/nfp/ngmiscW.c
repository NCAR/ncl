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
#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

NhlErrorTypes nggcog_W(void)
{

/*
 *  Input variables
 */

  float     *clat;
  int       ndims_clat, dsizes_clat[NCL_MAX_DIMENSIONS];
  float     *clon;
  int       ndims_clon, dsizes_clon[NCL_MAX_DIMENSIONS];
  float     *crad;
  int       ndims_crad, dsizes_crad[NCL_MAX_DIMENSIONS];

  int       has_missing_clat, has_missing_clon, has_missing_crad;
  NclScalar missing_clat, missing_clon, missing_crad;

/*
 *  Output variables
 */
  float     *alat;
  int       ndims_alat, dsizes_alat[NCL_MAX_DIMENSIONS];
  float     *alon;
  int       ndims_alon, dsizes_alon[NCL_MAX_DIMENSIONS];

  int       has_missing_alat, has_missing_alon;
  NclScalar missing_alat, missing_alon;

/*
 *  Local variables.
 */
  int       i, num_points;
  float     pnm;

/*
 *  Retrieve argument #1 (input)
 */
  clat = (float *) NclGetArgValue(
      0,
      5,
      &ndims_clat,
      dsizes_clat,
      &missing_clat,
      &has_missing_clat,
      NULL,
      2);

/*
 *  Retrieve argument #2 (input)
 */
  clon = (float *) NclGetArgValue(
      1,
      5,
      &ndims_clon,
      dsizes_clon,
      &missing_clon,
      &has_missing_clon,
      NULL,
      2);

/*
 *  Retrieve argument #3 (input)
 */
  crad = (float *) NclGetArgValue(
      2,
      5,
      &ndims_crad,
      dsizes_crad,
      &missing_crad,
      &has_missing_crad,
      NULL,
      2);

/*
 *  Retrieve argument #4 (output)
 */
  alat = (float *) NclGetArgValue(
       3,
       5,
       &ndims_alat,
       dsizes_alat,
       &missing_alat,
       &has_missing_alat,
       NULL,
       1);

/*
 *  Retrieve argument #5 (output)
 */
  alon = (float *) NclGetArgValue(
       4,
       5,
       &ndims_alon,
       dsizes_alon,
       &missing_alon,
       &has_missing_alon,
       NULL,
       1);

/*
 *  Check that the two output arrays are of the same size.
 */
  if (dsizes_alat[0] != dsizes_alon[0]) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "nggcog: output arrays must be of the same size");
         return(NhlFATAL);
  }
  else {
        num_points = dsizes_alat[0];
  }

/*
 *  Make the call to c_nggcog.
 */
    c_nggcog(*clat, *clon, *crad, alat, alon, num_points);

}

#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(mapgci,MAPGCI)(float *,float *,float *,float *,int *,
                                   float *,float *);


NhlErrorTypes great_circle_W( void )
{
/*
 * Input variables
 */
  void *lat1, *lon1, *lat2, *lon2;
  int ndims_lat1, ndims_lon1, ndims_lat2, ndims_lon2;
  int dsizes_lat1[NCL_MAX_DIMENSIONS], dsizes_lat2[NCL_MAX_DIMENSIONS];
  int dsizes_lon1[NCL_MAX_DIMENSIONS], dsizes_lon2[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_lat1, type_lon1, type_lat2, type_lon2;
/*
 * Output variables.
 */
  float *latlon;
  int dsizes_latlon[2];
/*
 * Other variables
 */
  int i, j, *nlatlon;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  lat1 = (void*)NclGetArgValue(
          0,
          5,
          &ndims_lat1,
          dsizes_lat1,
          NULL,
          NULL,
          &type_lat1,
          2);

  lon1 = (void*)NclGetArgValue(
          1,
          5,
          &ndims_lon1,
          dsizes_lon1,
          NULL,
          NULL,
          &type_lon1,
          2);

  lat2 = (void*)NclGetArgValue(
          2,
          5,
          &ndims_lat2,
          dsizes_lat2,
          NULL,
          NULL,
          &type_lat2,
          2);

  lon2 = (void*)NclGetArgValue(
          3,
          5,
          &ndims_lon2,
          dsizes_lon2,
          NULL,
          NULL,
          &type_lon2,
          2);

  nlatlon = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Allocate space for output array.
 */
  latlon = (float*)calloc(*nlatlon*2,sizeof(float));
  if(latlon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"great_circle: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  dsizes_latlon[0] = 2;
  dsizes_latlon[1] = *nlatlon;

/*
 * Call Fortran function.
 */
  NGCALLF(mapgci,MAPGCI)(lat1,lon1,lat2,lon2,nlatlon,
                         &latlon[0],&latlon[*nlatlon]);
/*
 * Return double values with missing value set.
 */
  return(NclReturnValue(latlon,2,dsizes_latlon,NULL,NCL_float,0));
}


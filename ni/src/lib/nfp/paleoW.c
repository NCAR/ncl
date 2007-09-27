#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(paleooutline,PALEOOUTLINE)(double*,float*,double*,double*,
                                               int*,int*,int*,int*,int*,int*,
                                               char*,float*,int);
NhlErrorTypes paleo_outline_W( void )
{
/*
 * Input array variables
 */
  void *oro, *lat, *lon;
  float *landmask;
  double *tmp_oro, *tmp_lat, *tmp_lon;
  int dsizes_oro[2], dsizes_lat[1], dsizes_lon[1];
  NclBasicDataTypes type_oro, type_lat, type_lon;
  string *name;
/*
 * Other variables
 */
  float *zdat;
  char *cname;
  int *iwrk, liwk, nlat, nlon, jm, im;
/*
 * Retrieve arguments.
 */
  oro = (void*)NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_oro,
          NULL,
          NULL,
          &type_oro,
          2);

  lat = (void*)NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_lat,
          NULL,
          NULL,
          &type_lat,
          2);

  lon = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_lon,
          NULL,
          NULL,
          &type_lon,
          2);

  landmask = (float*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  name = (string*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  cname = NrmQuarkToString(*name);

  nlat = dsizes_oro[0];
  nlon = dsizes_oro[1];
  if(dsizes_lat[0] != nlat || dsizes_lon[0] != nlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"paleo_outline: the length of the lat array must be the same as the leftmost dimension of oro, and the length of the lon arrays must be the same as the rightmost dimension of oro");
    return(NhlFATAL);
  }
/*
 * Convert input arrays to double if necessary.
 */
  tmp_oro = coerce_input_double(oro,type_oro,nlat*nlon,0,NULL,NULL);
  tmp_lat = coerce_input_double(lat,type_lat,nlat,0,NULL,NULL);
  tmp_lon = coerce_input_double(lon,type_lon,nlon,0,NULL,NULL);
  if(tmp_oro == NULL || tmp_lat == NULL || tmp_lon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"paleo_outline: Unable to coerce input arrays to double precision"); 
    return(NhlFATAL);
  }
/*
 * Allocate space for work arrays.
 */
  jm   = 2*nlat+1;
  im   = 2*nlon+1;
  liwk = max(im * jm,2000);         /* 2000 is the old value that iwrk 
                                       was hard-wired to. */

  zdat = (float*)malloc(jm*im*sizeof(float));
  iwrk = (int*)malloc(liwk*sizeof(int));
  if(zdat == NULL || iwrk == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"paleo_outline: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Call the Fortran paleo_outline routine.
 */
  NGCALLF(paleooutline,PALEOOUTLINE)(tmp_oro,zdat,tmp_lat,tmp_lon,
                                     &nlat,&nlon,&jm,&im,iwrk,&liwk,
                                     cname,landmask,strlen(cname));
  if(type_oro != NCL_double) NclFree(tmp_oro);
  if(type_lat != NCL_double) NclFree(tmp_lat);
  if(type_lon != NCL_double) NclFree(tmp_lon);

  NclFree(zdat);
  NclFree(iwrk);

  return(NhlNOERROR);
}

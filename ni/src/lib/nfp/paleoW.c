#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(paleooutline,PALEOOUTLINE)(float*,float*,int*,int*,
                                               int*,int*,char*,float*);

NhlErrorTypes paleo_outline_W( void )
{
/*
 * Input array variables
 */
  float *oro, *landmask;
  int dsizes_oro[2];
  string *name;
/*
 * Other variables
 */
  float *zdat;
  char *cname;
  int nlat, nlon, jm, im;
/*
 * Retrieve arguments.
 */
  oro = (float*)NclGetArgValue(
          0,
          3,
          NULL,
          dsizes_oro,
          NULL,
          NULL,
          NULL,
          2);

  landmask = (float*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  name = (string*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  cname = NrmQuarkToString(*name);

  nlat = dsizes_oro[0];
  nlon = dsizes_oro[1];
  jm   = 2*nlat;
  im   = 2*nlon;
/*
 * Allocate space for work array.
 */
  zdat = (float*)calloc(jm*im,sizeof(float));
  if( zdat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"paleo_outline: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran paleo_outline routine.
 */
  NGCALLF(paleooutline,PALEOOUTLINE)(oro,zdat,&nlat,&nlon,&jm,&im,cname,
                                     landmask);

  NclFree(zdat);

  return(NhlNOERROR);
}



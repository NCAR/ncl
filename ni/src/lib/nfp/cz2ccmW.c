#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>
#include "wrapper.h"

extern void NGCALLF(dcz2ccm,DCZ2CCM)(double*,double*,double*,double*,double*,
                                     double*,double*,double*,int*,int*,int*,
                                     int*,double*,double*,double*,double*,
                                     double*,double*,double*,double*,double*);

NhlErrorTypes cz2ccm_W( void )
{
/*
 * Input array variables
 */
  void *ps, *phis, *tv, *p0, *hyam, *hybm, *hyai, *hybi;
  double *dps, *dphis, *dtv, *dp0;
  double *dhyam, *dhybm, *dhyai, *dhybi;
  int has_missing_ps, ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int has_missing_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int has_missing_tv, ndims_tv, dsizes_tv[NCL_MAX_DIMENSIONS];
  int has_missing_hyam, dsizes_hyam[NCL_MAX_DIMENSIONS];
  int has_missing_hybm, dsizes_hybm[NCL_MAX_DIMENSIONS];
  int has_missing_hyai, dsizes_hyai[NCL_MAX_DIMENSIONS];
  int has_missing_hybi, dsizes_hybi[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ps, type_phis, type_tv, type_p0;
  NclBasicDataTypes type_hyam, type_hybm, type_hyai, type_hybi;
/*
 * Output array variables
 */
  double *z2;
  float *rz2;
  int size_leftmost;
/*
 * work arrays
 */
  double *pmln, *hypdln, *hyalph, *zslice, *hyba, *hybb, *pterm, *tv2;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, l, m, nlat, mlon, klev, klev1, nlatmlon, klevnlatmlon;
  int any_double=0, size_input, size_z2;

/*
 * Retrieve arguments.
 */
  ps = (void*)NclGetArgValue(
          0,
          8,
          &ndims_ps,
          dsizes_ps,
          NULL,
          &has_missing_ps,
          &type_ps,
          2);

  phis = (void*)NclGetArgValue(
          1,
          8,
          NULL,
          dsizes_phis,
          NULL,
          &has_missing_phis,
          &type_phis,
          2);

  tv = (void*)NclGetArgValue(
          2,
          8,
          &ndims_tv,
          dsizes_tv,
          NULL,
          &has_missing_tv,
          &type_tv,
          2);

  p0 = (void*)NclGetArgValue(
          3,
          8,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p0,
          2);

  hyam = (void*)NclGetArgValue(
          4,
          8,
          NULL,
          dsizes_hyam,
          NULL,
          &has_missing_hyam,
          &type_hyam,
          2);

  hybm = (void*)NclGetArgValue(
          5,
          8,
          NULL,
          dsizes_hybm,
          NULL,
          &has_missing_hybm,
          &type_hybm,
          2);

  hyai = (void*)NclGetArgValue(
          6,
          8,
          NULL,
          dsizes_hyai,
          NULL,
          &has_missing_hyai,
          &type_hyai,
          2);

  hybi = (void*)NclGetArgValue(
          7,
          8,
          NULL,
          dsizes_hybi,
          NULL,
          &has_missing_hybi,
          &type_hybi,
          2);
/*
 * ps must be at least two dimensions.
 */
  if( ndims_ps < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The input array 'ps' must be at least 2 dimensions");
    return(NhlFATAL);
  }
/*
 * tv must have the same dimensions as ps, only with one more dimension
 * 'klev'.
 */
  if( ndims_tv != ndims_ps+1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The input array 'tv' must have one more dimension than 'ps'");
    return(NhlFATAL);
  }
/*
 * Get nlat, mlon, klev, klev1.
 */ 
  nlat = dsizes_ps[ndims_ps-2];
  mlon = dsizes_ps[ndims_ps-1];
  klev = dsizes_tv[ndims_tv-3];
  klev1 = dsizes_hyai[0];
  nlatmlon = nlat * mlon;
  klevnlatmlon = klev * nlatmlon;
/*
 * Check dimension sizes of phis.
 */
  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The dimensions of 'phis' must be nlat x mlon");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes of tv.
 */
  if( dsizes_tv[ndims_tv-1] != mlon || dsizes_tv[ndims_tv-2] != nlat ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The last two dimensions of tv must be nlat x mlon");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes of tv and ps.
 */
  for( i = 0; i < ndims_tv-3; i++ ) {
    if( dsizes_tv[i] != dsizes_ps[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The rightmost dimensions of 'ps' and 'tv' must be the same");
      return(NhlFATAL);
    }
  }
/*
 * hyam and hybm must be the same dimension sizes and
 * hyai and hybi must be the same dimension sizes.
 */
  if( dsizes_hyam[0] != klev || dsizes_hybm[0] != klev ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The dimension size of 'hyam' and 'hybm' must be 'klev'");
    return(NhlFATAL);
  }

  if( dsizes_hybi[0] != klev1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The dimension size of 'hyai' and 'hybi' must be 'klev1'");
    return(NhlFATAL);
  }
  
/*
 * None of the input arrays can contain missing values.  Just print out
 * a warning message.
 */
  if( has_missing_ps ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cz2ccm: The array 'ps' cannot contain any missing values");
  }
  if( has_missing_phis ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cz2ccm: The array 'phis' cannot contain any missing values");
  }

  if( has_missing_tv ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cz2ccm: The array 'tv' cannot contain any missing values");
  }

  if( has_missing_hyam || has_missing_hybm ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cz2ccm: The arrays 'hyam' and 'hybm' cannot contain any missing values");
  }
  if( has_missing_hyai || has_missing_hybi ) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"cz2ccm: The arrays 'hyai' and 'hybi' cannot contain any missing values");
  }
/*
 * Compute the size of the leftmost dimensions of the output array
 * (minus the nlat,mlon,klev dims).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_tv-3; i++ ) size_leftmost *= dsizes_tv[i];
  size_z2 = size_leftmost*klev*nlat*mlon;
/*
 * Coerce ps to double precision if necessary.
 */
  size_input = size_leftmost * nlatmlon;
  dps = coerce_input_double(ps,type_ps,size_input,0,NULL,NULL,NULL);
  if( dps == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing ps array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce phis to double precision if necessary.
 */
  size_input = nlatmlon;
  dphis = coerce_input_double(phis,type_phis,size_input,0,NULL,NULL,NULL);
  if( dphis == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing phis array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce tv to double precision if necessary.
 */
  size_input = size_leftmost * klevnlatmlon;
  dtv = coerce_input_double(tv,type_tv,size_input,0,NULL,NULL,NULL);
  if( dtv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing tv array double precision");
    return(NhlFATAL);
  }

/*
 * Coerce p0 to double precision if necessary.
 */
  dp0 = coerce_input_double(p0,type_p0,1,0,NULL,NULL,NULL);
  if( dp0 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing p0 to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce hyam to double precision if necessary.
 */
  dhyam = coerce_input_double(hyam,type_hyam,klev,0,NULL,NULL,NULL);
  if( dhyam == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hyam array to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce hybm to double precision if necessary.
 */
  dhybm = coerce_input_double(hybm,type_hybm,klev,0,NULL,NULL,NULL);
  if( dhybm == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hybm array to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce hyai to double precision if necessary.
 */
  dhyai = coerce_input_double(hyai,type_hyai,klev1,0,NULL,NULL,NULL);
  if( dhyai == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hyai array to double precision");
    return(NhlFATAL);
  }

/*
 * Coerce hybi to double precision if necessary.
 */
  dhybi = coerce_input_double(hybi,type_hybi,klev1,0,NULL,NULL,NULL);
  if( dhybi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hybi array to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output value.
 */
  z2 = (double *)NclMalloc(size_z2*sizeof(double));
  if( z2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for scratch arrays.
 */
  tv2    = (double *)NclMalloc(    klev*mlon*sizeof(double));
  pmln   = (double *)NclMalloc((klev+1)*mlon*sizeof(double));
  hypdln = (double *)NclMalloc(    klev*mlon*sizeof(double));
  hyalph = (double *)NclMalloc(    klev*mlon*sizeof(double));
  zslice = (double *)NclMalloc(    klev*mlon*sizeof(double));
  hyba   = (double *)NclMalloc(   2*(klev+1)*sizeof(double));
  hybb   = (double *)NclMalloc(   2*(klev+1)*sizeof(double));
  pterm  = (double *)NclMalloc(    klev*mlon*sizeof(double));
  if( pmln == NULL || hypdln == NULL || hyalph == NULL || zslice == NULL ||
      hyba == NULL ||   hybb == NULL ||  pterm == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 */
  j = k = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    NGCALLF(dcz2ccm,DCZ2CCM)(&dps[j],dphis,&dtv[k],dp0,
                             dhyam,dhybm,dhyai,dhybi,
                             &mlon,&nlat,&klev,&klev1,&z2[k],pmln,
                             hypdln,hyalph,zslice,hyba,hybb,pterm,tv2);
    j += nlatmlon;
    k += klevnlatmlon;
  }
/*
 * Free memory.
 */
  NclFree(tv2);
  NclFree(pmln);
  NclFree(hypdln);
  NclFree(hyalph);
  NclFree(zslice);
  NclFree(hyba);
  NclFree(hybb);
  NclFree(pterm);

  if((void*)dps   != ps)   NclFree(dps);
  if((void*)dphis != phis) NclFree(dphis);
  if((void*)dtv   != tv)   NclFree(dtv);
  if((void*)dp0   != p0)   NclFree(dp0);
  if((void*)dhyam != hyam) NclFree(dhyam);
  if((void*)dhybm != hybm) NclFree(dhybm);
  if((void*)dhyai != hyai) NclFree(dhyai);
  if((void*)dhybi != hybi) NclFree(dhybi);
        
  if(type_ps != NCL_double && type_phis != NCL_double && 
     type_tv != NCL_double && type_p0 != NCL_double && 
     type_hyam != NCL_double && type_hybm != NCL_double &&
     type_hyai != NCL_double && type_hybi != NCL_double) {
/*
 * None of the input was double, so return a float.
 */
    rz2 = coerce_output_float(z2,NULL,size_z2,0);
    if( rz2 == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
/*
 * return float array
 */
    return(NclReturnValue((void*)rz2,ndims_tv,dsizes_tv,NULL,NCL_float,0));
  }
  else {
/*
 * At least one of the input arrays was double, so return a double.
 */
    return(NclReturnValue((void*)z2,ndims_tv,dsizes_tv,NULL,NCL_double,0));
  }
}

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
  int size_z2;
/*
 * work arrays
 */
  double *pmln, *hypdln, *hyalph, *zslice, *hyba, *hybb, *pterm, *tv2;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, l, m, nlat, mlon, klev, klev1;
  int any_double=0, size_input, size_output;

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
 * Compute the total size of the output array (minus the nlat,mlon,klev dims).
 */
  size_z2 = 1;
  for( i = 0; i < ndims_tv-3; i++ ) {
    size_z2 *= dsizes_tv[i];
  }
  size_output = size_z2*klev*nlat*mlon;
/*
 * Coerce input to double precision if necessary.
 */
  if(type_ps != NCL_double) {
    size_input = size_z2 * nlat * mlon;
    dps = (double*)NclMalloc(sizeof(double)*size_input);
    if( dps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dps,
               ps,
               size_input,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ps)));
  }
  else {
    any_double = 1;
    dps = (double*)ps;
  }

  if(type_phis != NCL_double) {
    size_input = mlon*nlat;
    dphis = (double*)NclMalloc(sizeof(double)*size_input);
    if( dphis == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing phis array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dphis,
               phis,
               size_input,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_phis)));
  }
  else {
    any_double = 1;
    dphis = (double*)phis;
  }

  if(type_tv != NCL_double) {
    size_input = size_z2 * klev * nlat * mlon;
    dtv = (double*)NclMalloc(sizeof(double)*size_input);
    if( dtv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing tv array double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dtv,
               tv,
               size_input,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_tv)));
  }
  else {
    any_double = 1;
    dtv = (double*)tv;
  }

  if(type_p0 != NCL_double) {
    dp0 = (double*)NclMalloc(sizeof(double));
    if( dp0 == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing p0 to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dp0,
               p0,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_p0)));
  }
  else {
    any_double = 1;
    dp0 = (double*)p0;
  }

  if(type_hyam != NCL_double) {
    dhyam = (double*)NclMalloc(sizeof(double)*klev);
    if( dhyam == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hyam array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dhyam,
               hyam,
               klev,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_hyam)));
  }
  else {
    any_double = 1;
    dhyam = (double*)hyam;
  }

  if(type_hybm != NCL_double) {
    dhybm = (double*)NclMalloc(sizeof(double)*klev);
    if( dhybm == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hybm array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dhybm,
               hybm,
               klev,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_hybm)));
  }
  else {
    any_double = 1;
    dhybm = (double*)hybm;
  }

  if(type_hyai != NCL_double) {
    dhyai = (double*)NclMalloc(sizeof(double)*klev1);
    if( dhyai == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hyai array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dhyai,
               hyai,
               klev1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_hyai)));
  }
  else {
    any_double = 1;
    dhyai = (double*)hyai;
  }

  if(type_hybi != NCL_double) {
    dhybi = (double*)NclMalloc(sizeof(double)*klev1);
    if( dhybi == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for coercing hybi array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dhybi,
               hybi,
               klev1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_hybi)));
  }
  else {
    any_double = 1;
    dhybi = (double*)hybi;
  }
/*
 * Allocate space for output value.
 */
  z2 = (double *)NclMalloc(size_output*sizeof(double));
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
      hyba == NULL || hybb == NULL || pterm == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of this routine.
 *
 */
  j = k = 0;
  for( i = 0; i < size_z2; i++ ) {
    NGCALLF(dcz2ccm,DCZ2CCM)(&dps[j],dphis,&dtv[k],dp0,
                             dhyam,dhybm,dhyai,dhybi,
                             &mlon,&nlat,&klev,&klev1,&z2[k],pmln,
                             hypdln,hyalph,zslice,hyba,hybb,pterm,tv2);
    j += nlat*mlon;
    k += klev*nlat*mlon;
  }
/*
 * Free memory.
 */
  free((double *)tv2);
  free((double *)pmln);
  free((double *)hypdln);
  free((double *)hyalph);
  free((double *)zslice);
  free((double *)hyba);
  free((double *)hybb);
  free((double *)pterm);

  if((void*)dps != ps) {
    NclFree(dps);
  }

  if((void*)dphis != phis) {
    NclFree(dphis);
  }

  if((void*)dtv != tv) {
    NclFree(dtv);
  }

  if((void*)dp0 != p0) {
    NclFree(dp0);
  }

  if((void*)dhyam != hyam) {
    NclFree(dhyam);
  }
  if((void*)dhybm != hybm) {
    NclFree(dhybm);
  }

  if((void*)dhyai != hyai) {
    NclFree(dhyai);
  }

  if((void*)dhybi != hybi) {
    NclFree(dhybi);
  }
        
  if(!any_double) {
/*
 * None of the input was double, so return a float.
 */
    rz2 = (float *)NclMalloc(size_output*sizeof(float));
    if( rz2 == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for return array");
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < size_output; i++ ) {
      rz2[i] = (float)z2[i];
    }
/*
 * Free up double array.
 */
    NclFree(z2);
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

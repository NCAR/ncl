#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>

NhlErrorTypes cz2ccm_W( void )
{
/*
 * Input array variables
 */
  float *ps, *phis, *tv, *p0, *hyam, *hybm, *hyai, *hybi;
  int has_missing_ps, ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int has_missing_phis, ndims_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int has_missing_tv, ndims_tv, dsizes_tv[NCL_MAX_DIMENSIONS];
  int ndims_p0, dsizes_p0[NCL_MAX_DIMENSIONS];
  int has_missing_hyam, ndims_hyam, dsizes_hyam[NCL_MAX_DIMENSIONS];
  int has_missing_hybm, ndims_hybm, dsizes_hybm[NCL_MAX_DIMENSIONS];
  int has_missing_hyai, ndims_hyai, dsizes_hyai[NCL_MAX_DIMENSIONS];
  int has_missing_hybi, ndims_hybi, dsizes_hybi[NCL_MAX_DIMENSIONS];
/*
 * Output array variables
 */
  float *z2;
  int size_z2;
/*
 * work arrays
 */
  float *pmln, *hypdln, *hyalph, *zslice, *hyba, *hybb, *pterm, *tv2;
/*
 * Declare various variables for random purposes.
 */
  int i, j, k, l, m, nlat, mlon, klev, klev1;

/*
 * Retrieve arguments.
 */
  ps = (float*)NclGetArgValue(
          0,
          8,
          &ndims_ps,
          dsizes_ps,
		  NULL,
		  &has_missing_ps,
          NULL,
          2);

  phis = (float*)NclGetArgValue(
          1,
          8,
          &ndims_phis,
          dsizes_phis,
		  NULL,
		  &has_missing_phis,
          NULL,
          2);

  tv = (float*)NclGetArgValue(
          2,
          8,
          &ndims_tv,
          dsizes_tv,
		  NULL,
		  &has_missing_tv,
          NULL,
          2);

  p0 = (float*)NclGetArgValue(
          3,
          8,
          &ndims_p0,
          dsizes_p0,
		  NULL,
		  NULL,
          NULL,
          2);

  hyam = (float*)NclGetArgValue(
          4,
          8,
          &ndims_hyam,
          dsizes_hyam,
          NULL,
          &has_missing_hyam,
          NULL,
          2);

  hybm = (float*)NclGetArgValue(
          5,
          8,
          &ndims_hybm,
          dsizes_hybm,
          NULL,
          &has_missing_hybm,
          NULL,
          2);

  hyai = (float*)NclGetArgValue(
          6,
          8,
          &ndims_hyai,
          dsizes_hyai,
          NULL,
          &has_missing_hyai,
          NULL,
          2);

  hybi = (float*)NclGetArgValue(
          7,
          8,
          &ndims_hybi,
          dsizes_hybi,
          NULL,
          &has_missing_hybi,
          NULL,
          2);
/*
 * ps must be at least two dimensions.
 * phis can only be two dimensions.
 */
  if( ndims_ps < 2 || ndims_phis != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The input array 'ps' must be at least 2 dimensions and 'phis' can only be two dimensions'");
    return(NhlFATAL);
  }
/*
 * tv must have the same dimensions as ps, only with one more dimension 'klev'.
 */
  if( ndims_tv != ndims_ps+1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The input array 'tv' must have one more dimension than 'ps'");
    return(NhlFATAL);
  }
  nlat = dsizes_ps[ndims_ps-2];
  mlon = dsizes_ps[ndims_ps-1];
  klev = dsizes_tv[ndims_tv-3];

  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The dimensions of 'phis' must be nlat x mlon");
	return(NhlFATAL);
  }
  
  if( dsizes_tv[ndims_tv-1] != mlon || dsizes_tv[ndims_tv-2] != nlat ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The last two dimensions of tv must be nlat x mlon");
	return(NhlFATAL);
  }

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
  if( ndims_hyam != 1 || ndims_hybm != 1 ||
	  ndims_hyai != 1 || ndims_hybi != 1 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The arrays 'hyam', 'hybm', 'hyai', and 'hybi' must be one-dimensional");
	return(NhlFATAL);
  }

  if( dsizes_hyam[0] != klev || dsizes_hybm[0] != klev ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: The dimension size of 'hyam' and 'hybm' must be 'klev'");
	return(NhlFATAL);
  }

  klev1 = dsizes_hyai[0];
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

/*
 * Allocate space for output value.
 */
  z2 = (float *)NclMalloc(size_z2*klev*nlat*mlon*sizeof(float));
  if( z2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cz2ccm: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for scratch arrays.
 */
  tv2    = (float *)NclMalloc(    klev*mlon*sizeof(float));
  pmln   = (float *)NclMalloc((klev+1)*mlon*sizeof(float));
  hypdln = (float *)NclMalloc(    klev*mlon*sizeof(float));
  hyalph = (float *)NclMalloc(    klev*mlon*sizeof(float));
  zslice = (float *)NclMalloc(    klev*mlon*sizeof(float));
  hyba   = (float *)NclMalloc(   2*(klev+1)*sizeof(float));
  hybb   = (float *)NclMalloc(   2*(klev+1)*sizeof(float));
  pterm  = (float *)NclMalloc(    klev*mlon*sizeof(float));
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
	NGCALLF(cz2ccm, CZ2CCM)(&ps[j],phis,&tv[k],p0,hyam,hybm,hyai,hybi,
							&mlon,&nlat,&klev,&klev1,&z2[k],pmln,hypdln,
							hyalph,zslice,hyba,hybb,pterm,tv2);
	j += nlat*mlon;
	k += klev*nlat*mlon;
  }
/*
 * Free memory.
 */
  free((float *)tv2);
  free((float *)pmln);
  free((float *)hypdln);
  free((float *)hyalph);
  free((float *)zslice);
  free((float *)hyba);
  free((float *)hybb);
  free((float *)pterm);

  return(NclReturnValue((void*)z2,ndims_tv,dsizes_tv,NULL,NCL_float,0));
}


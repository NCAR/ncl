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
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <math.h>
#include <ncarg/gks.h>

float NGCALLF(pslhy1, PSLHY1)();

NhlErrorTypes pslhyp_W( void )
{
/*
 * Input array variables
 */
  float *pres, *z, *tv;
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_tv, dsizes_tv[NCL_MAX_DIMENSIONS];
  int has_missing_pres;
  NclScalar missing_pres;
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  float *slp;
  int size_slp;
/*
 * Declare various variables for random purposes.
 */
  int i;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  pres = (float*)NclGetArgValue(
          0,
          3,
          &ndims_pres,
          dsizes_pres,
		  &missing_pres,
		  &has_missing_pres,
          NULL,
          2);

/*  
 * Check for a missing value in pres.
 */
  if( !has_missing_pres ) {
	missing_pres.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
/*
 * Retrieve argument #2
 */
  z = (float*)NclGetArgValue(
          1,
          3,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3
 */
  tv = (float*)NclGetArgValue(
          2,
          3,
          &ndims_tv,
          dsizes_tv,
          NULL,
          NULL,
          NULL,
          2);

/*
 * The input arrays must be at least two dimensions and the same dimension
 * sizes.
 */
  if( ndims_pres < 2 || ndims_z != ndims_pres || ndims_z != ndims_tv ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: The input arrays must be at least two dimensions and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_pres; i++ ) {
	if( dsizes_z[i] != dsizes_pres[i] || dsizes_z[i] != dsizes_tv[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: The input arrays must have the same dimension sizes");
	  return(NhlFATAL);
	}
  }
/*
 * Compute the total size of the output array.
 */
  size_slp = 1;
  for( i = 0; i < ndims_pres; i++ ) {
    size_slp *= dsizes_pres[i];
  }

/*
 * Allocate space for output value.
 */
  slp = (float *)NclMalloc(size_slp*sizeof(float));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_slp; i++ ) {
	slp[i] = NGCALLF(pslhy1, PSLHY1)(&pres[i],&z[i],&tv[i],&missing_pres.floatval);
  }
/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)slp,
                        &missing_pres,
                        ndims_pres,
                        dsizes_pres,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  long_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *long_name = NrmStringToQuark("sea level pressure");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         long_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "long_name",
             att_md,
             NULL
             );

  short_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *short_name = NrmStringToQuark("SLP");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         short_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "short_name",
             att_md,
             NULL
             );

  units = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark("Pa");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         units,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "units",
             att_md,
             NULL
             );
  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
} 

NhlErrorTypes pslec_W( void )
{
/*
 * Input array variables
 */
  float *t, *phis, *ps, *pres;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  float *slp;
  int size_slp;
/*
 * Declare various variables for random purposes.
 */
  int i, l, nlat, mlon;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  t = (float*)NclGetArgValue(
          0,
          4,
          &ndims_t,
          dsizes_t,
		  NULL,
		  NULL,
          NULL,
          2);

/*
 * Retrieve argument #2
 */
  phis = (float*)NclGetArgValue(
          1,
          4,
          &ndims_phis,
          dsizes_phis,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3
 */
  ps = (float*)NclGetArgValue(
          2,
          4,
          &ndims_ps,
          dsizes_ps,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  pres = (float*)NclGetArgValue(
          3,
          4,
          &ndims_pres,
          dsizes_pres,
		  NULL,
		  NULL,
          NULL,
          2);
/*
 * "phis" must only be two dimensions.
 */
  if( ndims_phis != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: The input array 'phis' must only be 2 dimensions");
    return(NhlFATAL);
  }
/*
 * The dimension sizes of the other three input variables must be the same
 * and must be at least two dimensions.
 */
  if( ndims_t < 2 || ndims_t != ndims_ps || ndims_t != ndims_pres ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: The input arrays 't', 'ps', and 'pres' must be at least 2 dimensions and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_t; i++ ) {
	if( dsizes_t[i] != dsizes_ps[i] || dsizes_t[i] != dsizes_pres[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: The input arrays 't', 'ps', and 'pres' must have the same dimension sizes");
	  return(NhlFATAL);
	}
  }
  nlat = dsizes_t[ndims_t-2];
  mlon = dsizes_t[ndims_t-1];
  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: The dimensions of 'phis' must be the same as the last two dimensions of 't'");
	return(NhlFATAL);
  }
/*
 * Compute the total size of the output array (minus the nlat,mlon dims).
 */
  size_slp = 1;
  for( i = 0; i < ndims_pres-2; i++ ) {
    size_slp *= dsizes_pres[i];
  }

/*
 * Allocate space for output value.
 */
  slp = (float *)NclMalloc(size_slp*nlat*mlon*sizeof(float));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  l = 0;
  for( i = 0; i < size_slp; i++ ) {
	NGCALLF(pslec, PSLEC)(&t[l],&phis[0],&ps[l],&pres[l],&mlon,&nlat,&slp[l]);
	l += nlat*mlon;
  }
/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)slp,
                        NULL,
                        ndims_pres,
                        dsizes_pres,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  long_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *long_name = NrmStringToQuark("sea level pressure");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         long_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "long_name",
             att_md,
             NULL
             );

  short_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *short_name = NrmStringToQuark("SLP");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         short_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "short_name",
             att_md,
             NULL
             );

  units = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark("Pa");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         units,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "units",
             att_md,
             NULL
             );
  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}

NhlErrorTypes pslhor_W( void )
{
/*
 * Input array variables
 */
  float *z, *t, *phis, *ps, *pres, *lats;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_lats, dsizes_lats[NCL_MAX_DIMENSIONS];
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  float *slp;
  int size_slp;
/*
 * Some extra arrays.
 */
  float *pslu, *zx, *tx, *presx;
/*
 * Declare various variables for random purposes.
 */
  int i, k, l, nlat, mlon, klev;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  z = (float*)NclGetArgValue(
          0,
          6,
          &ndims_z,
          dsizes_z,
		  NULL,
		  NULL,
          NULL,
          2);

/*
 * Retrieve argument #2
 */
  t = (float*)NclGetArgValue(
          1,
          6,
          &ndims_t,
          dsizes_t,
		  NULL,
		  NULL,
          NULL,
          2);

/*
 * Retrieve argument #3
 */
  phis = (float*)NclGetArgValue(
          2,
          6,
          &ndims_phis,
          dsizes_phis,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  ps = (float*)NclGetArgValue(
          3,
          6,
          &ndims_ps,
          dsizes_ps,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #5
 */
  pres = (float*)NclGetArgValue(
          4,
          6,
          &ndims_pres,
          dsizes_pres,
		  NULL,
		  NULL,
          NULL,
          2);

/*
 * Retrieve argument #6
 */
  lats = (float*)NclGetArgValue(
          5,
          6,
          &ndims_lats,
          dsizes_lats,
		  NULL,
		  NULL,
          NULL,
          2);

/*
 * Check the dimension sizes of the six input variables. z, t, and pres
 * must be the same size and at least 3 dimensions (... x klev x mlat x nlon),
 * ps must be mlat x nlon, phis must only be 2 dimensions (nlat x mlon, and
 * lats must only be one dimension (nlat).  z, t, pres, and ps can all have
 * additional dimensions, like time.
 */
  if( ndims_ps != ndims_z-1 || ndims_z < 3 || ndims_z != ndims_t || ndims_z != ndims_pres ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The input arrays 'z', 't', and 'pres' must have at least 3 dimensions and have the same number of dimensions, and the array 'ps' must have one less dimension than these arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_z; i++ ) {
	if( dsizes_z[i] != dsizes_pres[i] || dsizes_z[i] != dsizes_t[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The input arrays 'z', 't', and 'pres' must have the same dimensions");
	  return(NhlFATAL);
	}
  }

  for( i = 0; i < ndims_ps-2; i++ ) {
	if( dsizes_z[i] != dsizes_ps[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The rightmost dimension sizes of arrays 'z' and 'ps' must be the same");
	  return(NhlFATAL);
	}
  }
  klev = dsizes_z[ndims_z-3];
  nlat = dsizes_z[ndims_z-2];
  mlon = dsizes_z[ndims_z-1];

  if( dsizes_ps[ndims_ps-2] != nlat || dsizes_ps[ndims_ps-1] != mlon ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The last two dimension sizes of array 'ps' must be the same as the last two dimension sizes of array 'z'");
	return(NhlFATAL);
  }

  if( ndims_phis != 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The input array 'phis' must only be 2 dimensions");
    return(NhlFATAL);
  }

  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The dimension sizes of 'phis' must be the same as the last two dimension sizes of 'z'");
	return(NhlFATAL);
  }

  if( ndims_lats != 1 || dsizes_lats[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The input array 'lats' must have only one dimension and be the same length as the first dimension of 'phis'" );
    return(NhlFATAL);
  }

/*
 * Compute the total size of the output array (minus the nlat,mlon,klev dims).
 */
  size_slp = 1;
  for( i = 0; i < ndims_z-3; i++ ) {
    size_slp *= dsizes_z[i];
  }

/*
 * Allocate space for output value.
 */
  slp = (float *)NclMalloc(size_slp*nlat*mlon*sizeof(float));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  pslu  = (float *)NclMalloc(nlat*mlon*sizeof(float));
  zx    = (float *)NclMalloc(klev*mlon*sizeof(float));
  tx    = (float *)NclMalloc(klev*mlon*sizeof(float));
  presx = (float *)NclMalloc(klev*mlon*sizeof(float));
  if( pslu == NULL || zx == NULL || tx == NULL || presx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for arrays pslu, zx, tx, and/or presx");
    return(NhlFATAL);
  }

  k = l = 0;
  for( i = 0; i < size_slp; i++ ) {
	NGCALLF(pslhor, PSLHOR)(&z[k],&t[k],&phis[0],&ps[l],&pres[k],&lats[0],&mlon,&nlat,&klev,&slp[l],&pslu[0],&zx[0],&tx[0],&presx[0]);
	l += nlat*mlon;
	k += nlat*mlon*klev;
  }
/*
 * Free "extra" arrays.
 */
  free(pslu);
  free(zx);
  free(tx);
  free(presx);

/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)slp,
                        NULL,
                        ndims_ps,
                        dsizes_ps,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  long_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *long_name = NrmStringToQuark("sea level pressure");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         long_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "long_name",
             att_md,
             NULL
             );

  short_name = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *short_name = NrmStringToQuark("SLP");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         short_name,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "short_name",
             att_md,
             NULL
             );

  units = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *units = NrmStringToQuark("Pa");
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         units,
                         NULL,
                         1,
                         &nelem,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "units",
             att_md,
             NULL
             );
  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


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

extern double NGCALLF(dpslhy1,DPSLHY1)(double *,double *,double *,double *);
extern void NGCALLF(dpslec,DPSLEC)(double *,double *,double *,double *,int *,
                                  int *,double *);
extern void NGCALLF(dpslhor,DPSLHOR)(double *,double *,double *,double *,
                                    double *,double *,int *,int *,int *,
                                    double *,double *,double *,double *,
                                    double *);

NhlErrorTypes pslhyp_W( void )
{
/*
 * Input array variables
 */
  void    *pres,  *z,  *tv;
  double *dpres, *dz, *dtv;
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_z,    dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_tv,   dsizes_tv[NCL_MAX_DIMENSIONS];
  int has_missing_pres;
  NclScalar missing_pres, missing_dpres, missing_rpres;
  NclBasicDataTypes type_pres, type_z, type_tv;
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
  double *slp;
  float *rslp;
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
  pres = (void*)NclGetArgValue(
          0,
          3,
          &ndims_pres,
          dsizes_pres,
          &missing_pres,
          &has_missing_pres,
          &type_pres,
          2);

/*
 * Retrieve argument #2
 */
  z = (void*)NclGetArgValue(
          1,
          3,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          2);

/*
 * Retrieve argument #3
 */
  tv = (void*)NclGetArgValue(
          2,
          3,
          &ndims_tv,
          dsizes_tv,
          NULL,
          NULL,
          &type_tv,
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
 * Check for missing values.
 */
  if(has_missing_pres) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dpres,
               &missing_pres,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));

    if(type_pres != NCL_double) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 &missing_rpres,
                 &missing_pres,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));
    }
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_pres != NCL_double) {
      missing_rpres.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_dpres.doubleval = (double)missing_rpres.floatval;
    }
    else {
      missing_dpres.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Compute the total size of the output array.
 */
  size_slp = 1;
  for( i = 0; i < ndims_pres; i++ ) size_slp *= dsizes_pres[i];

/*
 * Coerce data to double if necessary.
 */
  if(type_pres != NCL_double) {
    dpres = (double*)NclMalloc(sizeof(double)*size_slp);
    if( dpres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_pres) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpres,
                 pres,
                 size_slp,
                 &missing_dpres,
                 &missing_pres,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dpres,
                 pres,
                 size_slp,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));
    }
  }
  else {
/*
 * pres is already double.
 */
    dpres = (double*)pres;
  }
/*
 * Coerce z array if necessary.
 */
  if(type_z != NCL_double) {
    dz = (double*)NclMalloc(sizeof(double)*size_slp);
    if( dz == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dz,
               z,
               size_slp,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_z)));
  }
  else {
/*
 * z is already double.
 */
    dz = (double*)z;
  }

/*
 * Coerce tv array if necessary.
 */
  if(type_tv != NCL_double) {
    dtv = (double*)NclMalloc(sizeof(double)*size_slp);
    if( dtv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing tv array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dtv,
               tv,
               size_slp,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_tv)));
  }
  else {
/*
 * tv is already double.
 */
    dtv = (double*)tv;
  }

/*
 * Allocate space for output value.
 */
  slp = (double *)NclMalloc(size_slp*sizeof(double));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  for( i = 0; i < size_slp; i++ ) {
    slp[i] = NGCALLF(dpslhy1,DPSLHY1)(&dpres[i],&dz[i],&dtv[i],
                                      &missing_dpres.doubleval);
  }
/*
 * free memory.
 */
  if((void*)dpres != pres) NclFree(dpres);
  if((void*)dz != z) NclFree(dz);
  if((void*)dtv != tv) NclFree(dtv);

/*
 * Set up variable to return.
 */
  if((type_pres != NCL_double) && (type_z != NCL_double) && 
     (  type_tv != NCL_double)) {
/*
 * None of the input is double, so return float values.
 */
    rslp = (float*)NclMalloc(sizeof(float)*size_slp);
    if( rslp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_slp; i++ ) rslp[i] = (float)slp[i];
/*
 * Free double precision values.
 */
    NclFree(slp);
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)rslp,
                        &missing_rpres,
                        ndims_pres,
                        dsizes_pres,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
  }
  else {
/*
 * At least one input array is double, so return double values.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)slp,
                        &missing_dpres,
                        ndims_pres,
                        dsizes_pres,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypedoubleClass
                        );
  }
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
  void *t, *phis, *ps, *pres;
  double *dt, *dphis, *dps, *dpres;
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_phis, type_ps, type_pres;
/*
 * Attribute variables
 */
  int att_id;
  int nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
  int size_pres, size_phis;
/*
 * Output array variables
 */
  double *slp;
  float *rslp;
  int size_slp1;
/*
 * Declare various variables for random purposes.
 */
  int i, l, nlat, mlon, any_double = 0;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  t = (void*)NclGetArgValue(
          0,
          4,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);

/*
 * Retrieve argument #2
 */
  phis = (void*)NclGetArgValue(
          1,
          4,
          &ndims_phis,
          dsizes_phis,
          NULL,
          NULL,
          &type_phis,
          2);

/*
 * Retrieve argument #3
 */
  ps = (void*)NclGetArgValue(
          2,
          4,
          &ndims_ps,
          dsizes_ps,
          NULL,
          NULL,
          &type_ps,
          2);

/*
 * Retrieve argument #4
 */
  pres = (void*)NclGetArgValue(
          3,
          4,
          &ndims_pres,
          dsizes_pres,
          NULL,
          NULL,
          &type_pres,
          2);
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
 * Compute sizes of input arrays.
 */
  size_pres = 1;
  for( i = 0; i < ndims_pres; i++ ) size_pres *= dsizes_pres[i];

  size_phis = 1;
  for( i = 0; i < ndims_phis; i++ ) size_phis *= dsizes_phis[i];

/*
 * Coerce t to double if necessary.
 */
  if(type_t != NCL_double) {
    dt = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dt,
               t,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_t)));
  }
  else {
    any_double = 1;
/*
 * t is already double.
 */
    dt = (double*)t;
  }

/*
 * Coerce phis to double if necessary.
 */
  if(type_phis != NCL_double) {
    dphis = (double*)NclMalloc(sizeof(double)*size_phis);
    if( dphis == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing phis array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dphis,
               phis,
               size_phis,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_phis)));
  }
  else {
    any_double = 1;
/*
 * phis is already double.
 */
    dphis = (double*)phis;
  }

/*
 * Coerce ps to double if necessary.
 */
  if(type_ps != NCL_double) {
    dps = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dps,
               ps,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ps)));
  }
  else {
    any_double = 1;
/*
 * ps is already double.
 */
    dps = (double*)ps;
  }

/*
 * Coerce pres to double if necessary.
 */
  if(type_pres != NCL_double) {
    dpres = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dpres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpres,
               pres,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));
  }
  else {
    any_double = 1;
/*
 * pres is already double.
 */
    dpres = (double*)pres;
  }

/*
 * Compute the total size of the output array (minus the nlat,mlon dims).
 */
  size_slp1 = 1;
  for( i = 0; i < ndims_pres-2; i++ ) size_slp1 *= dsizes_pres[i];

/*
 * Allocate space for output value.
 */
  slp = (double *)NclMalloc(size_pres*sizeof(double));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  l = 0;
  for( i = 0; i < size_slp1; i++ ) {
    NGCALLF(dpslec,DPSLEC)(&dt[l],&dphis[0],&dps[l],&dpres[l],&mlon,&nlat,
                           &slp[l]);
    l += nlat*mlon;
  }
/*
 * Free memory.
 */
  if((void*)dt != t) NclFree(dt);
  if((void*)dphis != phis) NclFree(dphis);
  if((void*)dps != ps) NclFree(dps);
  if((void*)dpres != pres) NclFree(dpres);

/*
 * Get ready to reeturn.
 */
  if(!any_double) {
/*
 * None of the input is double, so return float values.
 */
    rslp = (float*)NclMalloc(sizeof(float)*size_pres);
    if( rslp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_pres; i++ ) rslp[i] = (float)slp[i];
/*
 * Free double precision values.
 */
    NclFree(slp);
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rslp,
                              NULL,
                              ndims_pres,
                              dsizes_pres,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
  }
  else {
/*
 * At least one input array is double, so return double values.
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
                              (NclObjClass)nclTypedoubleClass
                              );
  }
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
  void *z, *t, *phis, *ps, *pres, *lats;
  double *dz, *dt, *dphis, *dps, *dpres, *dlats;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_t, dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis, dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps, dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres, dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_lats, dsizes_lats[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_z, type_t, type_phis, type_ps, type_pres, type_lats;
  int size_pres, size_ps, size_phis;
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
  double *slp;
  float *rslp;
  int size_slp, size_slp1;
/*
 * Some extra arrays.
 */
  double *pslu, *zx, *tx, *presx;
/*
 * Declare various variables for random purposes.
 */
  int i, k, l, nlat, mlon, klev, any_double = 0;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  z = (void*)NclGetArgValue(
          0,
          6,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          2);

/*
 * Retrieve argument #2
 */
  t = (void*)NclGetArgValue(
          1,
          6,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);

/*
 * Retrieve argument #3
 */
  phis = (void*)NclGetArgValue(
          2,
          6,
          &ndims_phis,
          dsizes_phis,
          NULL,
          NULL,
          &type_phis,
          2);

/*
 * Retrieve argument #4
 */
  ps = (void*)NclGetArgValue(
          3,
          6,
          &ndims_ps,
          dsizes_ps,
          NULL,
          NULL,
          &type_ps,
          2);

/*
 * Retrieve argument #5
 */
  pres = (void*)NclGetArgValue(
          4,
          6,
          &ndims_pres,
          dsizes_pres,
          NULL,
          NULL,
          &type_pres,
          2);

/*
 * Retrieve argument #6
 */
  lats = (void*)NclGetArgValue(
          5,
          6,
          &ndims_lats,
          dsizes_lats,
          NULL,
          NULL,
          &type_lats,
          2);

/*
 * Check the dimension sizes of the six input variables. z, t, and pres
 * must be the same size and at least 3 dimensions (... x klev x mlat x nlon),
 * ps must be mlat x nlon, phis must only be 2 dimensions (mlat x nlon), and
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

  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The dimension sizes of 'phis' must be the same as the last two dimension sizes of 'z'");
    return(NhlFATAL);
  }

  if( dsizes_lats[0] != nlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: The input array 'lats' must be the same length as the first dimension of 'phis'" );
    return(NhlFATAL);
  }
/*
 * Compute sizes of input arrays.
 */
  size_pres = 1;
  for( i = 0; i < ndims_pres; i++ ) size_pres *= dsizes_pres[i];

  size_phis = nlat*mlon;

  size_ps = 1;
  for( i = 0; i < ndims_ps; i++ ) size_ps *= dsizes_ps[i];

/*
 * Coerce z to double if necessary.
 */
  if(type_z != NCL_double) {
    dz = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dz == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dz,
               z,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_z)));
  }
  else {
    any_double = 1;
/*
 * z is already double.
 */
    dz = (double*)z;
  }

/*
 * Coerce t to double if necessary.
 */
  if(type_t != NCL_double) {
    dt = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dt,
               t,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_t)));
  }
  else {
    any_double = 1;
/*
 * t is already double.
 */
    dt = (double*)t;
  }

/*
 * Coerce phis to double if necessary.
 */
  if(type_phis != NCL_double) {
    dphis = (double*)NclMalloc(sizeof(double)*size_phis);
    if( dphis == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing phis array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dphis,
               phis,
               size_phis,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_phis)));
  }
  else {
    any_double = 1;
/*
 * phis is already double.
 */
    dphis = (double*)phis;
  }

/*
 * Coerce ps to double if necessary.
 */
  if(type_ps != NCL_double) {
    dps = (double*)NclMalloc(sizeof(double)*size_ps);
    if( dps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dps,
               ps,
               size_ps,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ps)));
  }
  else {
    any_double = 1;
/*
 * ps is already double.
 */
    dps = (double*)ps;
  }

/*
 * Coerce pres to double if necessary.
 */
  if(type_pres != NCL_double) {
    dpres = (double*)NclMalloc(sizeof(double)*size_pres);
    if( dpres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dpres,
               pres,
               size_pres,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_pres)));
  }
  else {
    any_double = 1;
/*
 * pres is already double.
 */
    dpres = (double*)pres;
  }

/*
 * Coerce lats to double if necessary.
 */
  if(type_lats != NCL_double) {
    dlats = (double*)NclMalloc(sizeof(double)*nlat);
    if( dlats == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing lats array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               dlats,
               lats,
               nlat,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_lats)));
  }
  else {
    any_double = 1;
/*
 * lats is already double.
 */
    dlats = (double*)lats;
  }

/*
 * Compute the total size of the output array (minus the nlat,mlon,klev dims).
 */
  size_slp1 = 1;
  for( i = 0; i < ndims_z-3; i++ ) size_slp1 *= dsizes_z[i];
  size_slp = size_slp1*nlat*mlon;

/*
 * Allocate space for output value.
 */
  slp = (double *)NclMalloc(size_slp*sizeof(double));
  if( slp == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate space for other output values.
 *
 */
  pslu  = (double *)NclMalloc(nlat*mlon*sizeof(double));
  zx    = (double *)NclMalloc(klev*mlon*sizeof(double));
  tx    = (double *)NclMalloc(klev*mlon*sizeof(double));
  presx = (double *)NclMalloc(klev*mlon*sizeof(double));
  if( pslu == NULL || zx == NULL || tx == NULL || presx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for arrays pslu, zx, tx, and/or presx");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  k = l = 0;
  for( i = 0; i < size_slp1; i++ ) {
    NGCALLF(dpslhor,DPSLHOR)(&dz[k],&dt[k],&dphis[0],&dps[l],&dpres[k],
                             &dlats[0],&mlon,&nlat,&klev,&slp[l],&pslu[0],
                             &zx[0],&tx[0],&presx[0]);
    l += nlat*mlon;
    k += nlat*mlon*klev;
  }
/*
 * Free memory.
 */
  if((void*)dt != t) NclFree(dt);
  if((void*)dz != z) NclFree(dz);
  if((void*)dphis != phis) NclFree(dphis);
  if((void*)dps != ps) NclFree(dps);
  if((void*)dpres != pres) NclFree(dpres);
  if((void*)dlats != lats) NclFree(dlats);

/*
 * Free "extra" arrays.
 */
  NclFree(pslu);
  NclFree(zx);
  NclFree(tx);
  NclFree(presx);

/*
 * Get ready to return.
 */
  if(!any_double) {
/*
 * None of the input is double, so return float values.
 */
    rslp = (float*)NclMalloc(sizeof(float)*size_slp);
    if( rslp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < size_slp; i++ ) rslp[i] = (float)slp[i];
/*
 * Free double precision values.
 */
    NclFree(slp);
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rslp,
                              NULL,
                              ndims_ps,
                              dsizes_ps,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
  }
  else {
/*
 * At least one input array is double, so return double values.
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
                              (NclObjClass)nclTypedoubleClass
                              );
  }
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


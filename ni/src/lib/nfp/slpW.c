#include <stdio.h>
#include <math.h>
#include "wrapper.h"

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
  void   *pres = NULL;
  void   *z = NULL;
  void   *tv = NULL;
  double *tmp_pres = NULL;
  double *tmp_z = NULL;
  double *tmp_tv = NULL;
  int ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_tv;
  ng_size_t dsizes_tv[NCL_MAX_DIMENSIONS];
  int has_missing_pres;
  NclScalar missing_pres, missing_dpres, missing_rpres;
  NclBasicDataTypes type_pres, type_z, type_tv;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
/*
 * Output array variables
 */
  void *slp;
  double *tmp_slp = NULL;
  ng_size_t size_slp;
  NclBasicDataTypes type_slp;
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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);
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
  for( i = 0; i < ndims_pres; i++ ) size_slp *= dsizes_pres[i];

/*
 * Coerce x and y missing values to double if necessary.
 */
  coerce_missing(type_pres,has_missing_pres,&missing_pres,&missing_dpres,
                 &missing_rpres);
/*
 * Coerce data to double if necessary.
 */
  if(type_pres != NCL_double) {
    tmp_pres = (double*)calloc(1,sizeof(double));
    if( tmp_pres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce z array if necessary.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(1,sizeof(double));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce tv array if necessary.
 */
  if(type_tv != NCL_double) {
    tmp_tv = (double*)calloc(1,sizeof(double));
    if( tmp_tv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for coercing tv array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output value.
 */
  if(type_pres != NCL_double && type_z != NCL_double && 
     type_tv != NCL_double) {

    type_slp = NCL_float;

    slp     = (void *)calloc(size_slp,sizeof(float));
    tmp_slp = (double *)calloc(1,sizeof(double));
    if(slp == NULL || tmp_slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
    slp = (void *)calloc(size_slp,sizeof(double));
    if(slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhyp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 */
  for( i = 0; i < size_slp; i++ ) {
    if(type_pres != NCL_double) {
/*
 * Coerce subsection of pres (tmp_pres) to double.
 */
      coerce_subset_input_double(pres,tmp_pres,i,type_pres,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_pres to appropriate location in pres.
 */
      tmp_pres = &((double*)pres)[i];
    }
    if(type_z != NCL_double) {
/*
 * Coerce subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,i,type_z,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[i];
    }
    if(type_tv != NCL_double) {
/*
 * Coerce subsection of tv (tmp_tv) to double.
 */
      coerce_subset_input_double(tv,tmp_tv,i,type_tv,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_tv to appropriate location in tv.
 */
      tmp_tv = &((double*)tv)[i];
    }
    if(type_slp == NCL_double) tmp_slp = &((double*)slp)[i];

    *tmp_slp = NGCALLF(dpslhy1,DPSLHY1)(tmp_pres,tmp_z,tmp_tv,
                                        &missing_dpres.doubleval);
/*
 * Copy output values from temporary tmp_slp to slp.
 */
    if(type_slp != NCL_double) {
      ((float*)slp)[i] = (float)(*tmp_slp);
    }
  }
/*
 * free memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_z    != NCL_double) NclFree(tmp_z);
  if(type_tv   != NCL_double) NclFree(tmp_tv);
  if(type_slp  != NCL_double) NclFree(tmp_slp);

/*
 * Set up variable to return.
 */
  if(type_slp != NCL_double) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        slp,
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
                        slp,
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
  double *tmp_t = NULL;
  double *tmp_phis = NULL;
  double *tmp_ps = NULL;
  double *tmp_pres = NULL;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis;
  ng_size_t dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps;
  ng_size_t dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t, type_phis, type_ps, type_pres;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;
  ng_size_t size_pres, size_phis;
/*
 * Output array variables
 */
  void *slp;
  double *tmp_slp = NULL;
  ng_size_t size_slp1;
  NclBasicDataTypes type_slp;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, l, nlat, mlon, nlatmlon;
  int inlat, imlon;
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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);
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
  nlatmlon = nlat*mlon;

  if( dsizes_phis[0] != nlat || dsizes_phis[1] != mlon ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: The dimensions of 'phis' must be the same as the last two dimensions of 't'");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes.
 */
  if((mlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: one or more input dimensions sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  imlon = (int) mlon;
  inlat = (int) nlat;

/*
 * Compute sizes of input arrays.
 */
  size_pres = 1;
  for( i = 0; i < ndims_pres; i++ ) size_pres *= dsizes_pres[i];

  size_phis = 1;
  for( i = 0; i < ndims_phis; i++ ) size_phis *= dsizes_phis[i];
/*
 * Compute the total size of the output array (minus the nlat,mlon dims).
 */
  size_slp1 = 1;
  for( i = 0; i < ndims_pres-2; i++ ) size_slp1 *= dsizes_pres[i];

/*
 * Coerce t to double if necessary.
 */
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(nlatmlon,sizeof(double));
    if( tmp_t == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
  }    
/*
 * Coerce phis to double if necessary.
 */
  tmp_phis = coerce_input_double(phis,type_phis,size_phis,0,NULL,NULL);
  if( tmp_phis == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing phis array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce ps to double if necessary.
 */
  if(type_ps != NCL_double) {
    tmp_ps = (double*)calloc(nlatmlon,sizeof(double));
    if( tmp_ps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Coerce pres to double if necessary.
 */
  if(type_pres != NCL_double) {
    tmp_pres = (double*)calloc(nlatmlon,sizeof(double));
    if( tmp_pres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output value.
 */
  if(type_t != NCL_double && type_phis != NCL_double && 
     type_ps != NCL_double && type_pres != NCL_double) {

    type_slp = NCL_float;
    tmp_slp = (double*)calloc(nlatmlon,sizeof(double));
    slp     = (void *)calloc(size_pres,sizeof(float));
    if(slp == NULL || tmp_slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
    slp = (void *)calloc(size_pres,sizeof(double));
    if( slp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslec: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  l = 0;
  for( i = 0; i < size_slp1; i++ ) {
    if(type_t != NCL_double) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
      coerce_subset_input_double(t,tmp_t,l,type_t,nlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[l];
    }
    if(type_ps != NCL_double) {
/*
 * Coerce subsection of ps (tmp_ps) to double.
 */
      coerce_subset_input_double(ps,tmp_ps,l,type_ps,nlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_ps to appropriate location in ps.
 */
      tmp_ps = &((double*)ps)[l];
    }
    if(type_pres != NCL_double) {
/*
 * Coerce subsection of pres (tmp_pres) to double.
 */
      coerce_subset_input_double(pres,tmp_pres,l,type_pres,nlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_pres to appropriate location in pres.
 */
      tmp_pres = &((double*)pres)[l];
    }

    if(type_slp == NCL_double) tmp_slp = &((double*)slp)[l];

    NGCALLF(dpslec,DPSLEC)(tmp_t,tmp_phis,tmp_ps,tmp_pres,&imlon,&inlat,
                           tmp_slp);
/*
 * Copy output values from temporary tmp_slp to slp.
 */
    if(type_slp != NCL_double) {
      for(j = 0; j < nlatmlon; j++ ) {
        ((float*)slp)[j+l] = (float)(tmp_slp[j]);
      }
    }
    l += nlatmlon;
  }
/*
 * Free memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_t    != NCL_double) NclFree(tmp_t);
  if(type_phis != NCL_double) NclFree(tmp_phis);
  if(type_ps   != NCL_double) NclFree(tmp_ps);
  if(type_slp  != NCL_double) NclFree(tmp_slp);

/*
 * Get ready to reeturn.
 */
  if(type_slp != NCL_double) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              slp,
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
                              slp,
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
  double *tmp_z = NULL;
  double *tmp_t = NULL;
  double *tmp_phis = NULL;
  double *tmp_ps = NULL;
  double *tmp_pres = NULL;
  double *tmp_lats = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  int ndims_phis;
  ng_size_t dsizes_phis[NCL_MAX_DIMENSIONS];
  int ndims_ps;
  ng_size_t dsizes_ps[NCL_MAX_DIMENSIONS];
  int ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  int ndims_lats;
  ng_size_t dsizes_lats[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_z, type_t, type_phis, type_ps, type_pres, type_lats;
  ng_size_t size_pres, size_ps, size_phis;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t nelem = 1;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  NclQuark *long_name, *short_name, *units;

/*
 * Output array variables
 */
  void *slp = NULL;
  double *tmp_slp = NULL;
  ng_size_t size_slp, size_slp1;
  NclBasicDataTypes type_slp;
/*
 * Some extra arrays.
 */
  double *pslu, *zx, *tx, *presx;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, k, l, nlat, mlon, klev, nlatmlon, klevnlatmlon;
  int inlat, imlon, iklev;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
          0,
          6,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);

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
          DONT_CARE);

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
  nlatmlon = nlat * mlon;
  klevnlatmlon = klev * nlatmlon;

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
 * Test dimension sizes.
 */
  if((mlon > INT_MAX) || (nlat > INT_MAX) || (klev > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: one or more input dimensions sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  imlon = (int) mlon;
  inlat = (int) nlat;
  iklev = (int) klev;

/*
 * Compute sizes of input arrays.
 */
  size_pres = 1;
  for( i = 0; i < ndims_pres; i++ ) size_pres *= dsizes_pres[i];

  size_phis = nlatmlon;

  size_ps = 1;
  for( i = 0; i < ndims_ps; i++ ) size_ps *= dsizes_ps[i];


/*
 * Compute the total size of the output array (minus the nlat,mlon,klev dims).
 */
  size_slp1 = 1;
  for( i = 0; i < ndims_z-3; i++ ) size_slp1 *= dsizes_z[i];
  size_slp = size_slp1*nlatmlon;

/*
 * Coerce z to double if necessary.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(klevnlatmlon,sizeof(double));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce t to double if necessary.
 */
  if(type_t != NCL_double) {
    tmp_t = (double*)calloc(klevnlatmlon,sizeof(double));
    if( tmp_t == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing t array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce phis to double if necessary.
 */
  tmp_phis = coerce_input_double(phis,type_phis,size_phis,0,NULL,NULL);
  if(tmp_phis == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing phis array to double precision");
    return(NhlFATAL);
  }
/*
 * Coerce ps to double if necessary.
 */
  if(type_ps != NCL_double) {
    tmp_ps = (double*)calloc(nlatmlon,sizeof(double));
    if(tmp_ps == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing ps array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce pres to double if necessary.
 */
  if(type_pres != NCL_double) {
    tmp_pres = (double*)calloc(klevnlatmlon,sizeof(double));
    if(tmp_pres == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing pres array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce lats to double if necessary.
 */
  tmp_lats = coerce_input_double(lats,type_lats,nlat,0,NULL,NULL);
  if( tmp_lats == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for coercing lats array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output value.
 */
  if(type_z != NCL_double && type_t != NCL_double && 
     type_phis != NCL_double && type_ps != NCL_double && 
     type_pres != NCL_double) {

    type_slp = NCL_float;

    tmp_slp = (double*)calloc(nlatmlon,sizeof(double));
    slp     = (void *)calloc(size_slp,sizeof(float));
    if(tmp_slp == NULL && slp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
    slp = (void *)calloc(size_slp,sizeof(double));
    if( slp == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for other output values.
 *
 */
  pslu  = (double *)calloc(nlatmlon,sizeof(double));
  zx    = (double *)calloc(klev*mlon,sizeof(double));
  tx    = (double *)calloc(klev*mlon,sizeof(double));
  presx = (double *)calloc(klev*mlon,sizeof(double));
  if( pslu == NULL || zx == NULL || tx == NULL || presx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pslhor: Unable to allocate memory for arrays pslu, zx, tx, and/or presx");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  k = l = 0;
  for( i = 0; i < size_slp1; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,k,type_z,klevnlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[k];
    }

    if(type_t != NCL_double) {
/*
 * Coerce subsection of t (tmp_t) to double.
 */
      coerce_subset_input_double(t,tmp_t,k,type_t,klevnlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[k];
    }

    if(type_pres != NCL_double) {
/*
 * Coerce subsection of pres (tmp_pres) to double.
 */
      coerce_subset_input_double(pres,tmp_pres,k,type_pres,klevnlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_pres to appropriate location in pres.
 */
      tmp_pres = &((double*)pres)[k];
    }

    if(type_ps != NCL_double) {
/*
 * Coerce subsection of ps (tmp_ps) to double.
 */
      coerce_subset_input_double(ps,tmp_ps,l,type_ps,nlatmlon,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_ps to appropriate location in ps.
 */
      tmp_ps = &((double*)ps)[l];
    }

    if(type_slp == NCL_double) tmp_slp = &((double*)slp)[l];


    NGCALLF(dpslhor,DPSLHOR)(tmp_z,tmp_t,tmp_phis,tmp_ps,tmp_pres,
                             tmp_lats,&imlon,&inlat,&iklev,tmp_slp,pslu,
                             zx,tx,presx);
/*
 * Copy output values from temporary tmp_slp to slp.
 */
    if(type_slp != NCL_double) {
      for(j = 0; j < nlatmlon; j++ ) {
        ((float*)slp)[j+l] = (float)(tmp_slp[j]);
      }
    }
    l += nlatmlon;
    k += klevnlatmlon;
  }
/*
 * Free memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_z    != NCL_double) NclFree(tmp_z);
  if(type_t    != NCL_double) NclFree(tmp_t);
  if(type_phis != NCL_double) NclFree(tmp_phis);
  if(type_ps   != NCL_double) NclFree(tmp_ps);
  if(type_lats != NCL_double) NclFree(tmp_lats);
  if(type_slp  != NCL_double) NclFree(tmp_slp);

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
  if(type_slp != NCL_double) {
/*
 * Set up return structure.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              slp,
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
                              slp,
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


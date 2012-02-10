#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(omcalcccm,OMCALCCCM)(double *u, double *v, 
                                         double *d, double *dpsl, 
                                         double *dpsm, double *pmid, 
                                         double *pdel, double *psfc, 
                                         double *hybd, double *hybm, 
                                         int *nprlev, double *omega, 
                                         int *ilon, int *jlat, int *klev);

NhlErrorTypes omega_ccm_W( void )
{
/*
 * Input array variables
 */
  void *u, *v, *div, *dpsl, *dpsm, *pmid, *pdel, *psfc, *hybd, *hybm;
  int *nprlev;
  double *tmp_u, *tmp_v, *tmp_div, *tmp_dpsl, *tmp_dpsm;
  double *tmp_pmid, *tmp_pdel, *tmp_psfc, *tmp_hybd, *tmp_hybm;
  int ndims_u, ndims_v, ndims_div, ndims_dpsl, ndims_dpsm;
  int ndims_pmid, ndims_pdel, ndims_psfc, ndims_hybd, ndims_hybm;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_div[NCL_MAX_DIMENSIONS], dsizes_dpsl[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_dpsm[NCL_MAX_DIMENSIONS], dsizes_pmid[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_pdel[NCL_MAX_DIMENSIONS], dsizes_psfc[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_hybd[1], dsizes_hybm[1];
  NclBasicDataTypes type_u, type_v, type_div, type_dpsl, type_dpsm;
  NclBasicDataTypes type_pmid, type_pdel, type_psfc, type_hybd, type_hybm;

/*
 * Output array variables
 */
  void *omega;
  double *tmp_omega;
  NclBasicDataTypes type_omega;

/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, nt, nlat, nlon, nlev, ntim, nlatlon, nlevlatlon, size_omega;
  ng_size_t index_u, index_psfc;
  int inlon, inlat, inlev;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  u = (void*)NclGetArgValue(
          0,
          11,
          &ndims_u,
          dsizes_u,
          NULL,
          NULL,
          &type_u,
          DONT_CARE);

  v = (void*)NclGetArgValue(
          1,
          11,
          &ndims_v,
          dsizes_v,
          NULL,
          NULL,
          &type_v,
          DONT_CARE);


  div = (void*)NclGetArgValue(
          2,
          11,
          &ndims_div,
          dsizes_div,
          NULL,
          NULL,
          &type_div,
          DONT_CARE);

  dpsl = (void*)NclGetArgValue(
          3,
          11,
          &ndims_dpsl,
          dsizes_dpsl,
          NULL,
          NULL,
          &type_dpsl,
          DONT_CARE);

  dpsm = (void*)NclGetArgValue(
          4,
          11,
          &ndims_dpsm,
          dsizes_dpsm,
          NULL,
          NULL,
          &type_dpsm,
          DONT_CARE);

  pmid = (void*)NclGetArgValue(
          5,
          11,
          &ndims_pmid,
          dsizes_pmid,
          NULL,
          NULL,
          &type_pmid,
          DONT_CARE);

  pdel = (void*)NclGetArgValue(
          6,
          11,
          &ndims_pdel,
          dsizes_pdel,
          NULL,
          NULL,
          &type_pdel,
          DONT_CARE);

  psfc = (void*)NclGetArgValue(
          7,
          11,
          &ndims_psfc,
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          DONT_CARE);

  hybd = (void*)NclGetArgValue(
          8,
          11,
          &ndims_hybd,
          dsizes_hybd,
          NULL,
          NULL,
          &type_hybd,
          DONT_CARE);

  hybm = (void*)NclGetArgValue(
          9,
          11,
          &ndims_hybm,
          dsizes_hybm,
          NULL,
          NULL,
          &type_hybm,
          DONT_CARE);

  nprlev = (int*)NclGetArgValue(
          10,
          11,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Check the input dimension sizes.
 */
  if(ndims_u != 3 && ndims_u != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'u' must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }

  if(ndims_v != 3 && ndims_v != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'v' must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }

  if(ndims_div != 3 && ndims_div != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'div' must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }

  if(ndims_pmid != 3 && ndims_pmid != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'pmid' must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }

  if(ndims_pdel != 3 && ndims_pdel != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'pdel' must be a 3 or 4 dimensional array");
    return(NhlFATAL);
  }

/*
 * Now check that the dimension sizes are equal to each other.
 */
  if(ndims_u != ndims_v    || ndims_u != ndims_div || 
     ndims_u != ndims_pmid || ndims_u != ndims_pdel) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: the first seven input variables must be the same dimensionality");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_u; i++) {
    if(dsizes_u[i] != dsizes_v[i]    || dsizes_u[i] != dsizes_div[i] || 
       dsizes_u[i] != dsizes_pmid[i] || dsizes_u[i] != dsizes_pdel[i]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: u, v, div, pmid, and pdel must be the same dimensionality");
    return(NhlFATAL);
    }
  }

  nlev = dsizes_u[ndims_u-3];
  nlat = dsizes_u[ndims_u-2];
  nlon = dsizes_u[ndims_u-1];
  if(ndims_u == 4) {
    ntim = dsizes_u[ndims_u-4];
  }
  else {
    ntim = 1;
  }

/*
 * Test input dimension sizes.
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX) || (nlev > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  inlev = (int) nlev;

/*
 * Test the sizes of 'dpsl', 'dpsm', and 'psfc' which should be one
 * dimension smaller than the others.
 */
  if(ndims_u == 3 && ndims_dpsl != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsl' must be a 2 dimensional array if the other input arguments are 3 dimensional");
    return(NhlFATAL);
  }

  if(ndims_u == 4 && ndims_dpsl != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsl' must be a 3 dimensional array if the other input arguments are 4 dimensional");
    return(NhlFATAL);
  }

  if(ndims_u == 3 && ndims_dpsm != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsm' must be a 2 dimensional array if the other input arguments are 3 dimensional");
    return(NhlFATAL);
  }

  if(ndims_u == 4 && ndims_dpsm != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsm' must be a 3 dimensional array if the other input arguments are 4 dimensional");
    return(NhlFATAL);
  }

  if(ndims_u == 3 && ndims_psfc != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'psfc' must be a 2 dimensional array if the other input arguments are 3 dimensional");
    return(NhlFATAL);
  }

  if(ndims_u == 4 && ndims_psfc != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'psfc' must be a 3 dimensional array if the other input arguments are 4 dimensional");
    return(NhlFATAL);
  }

/*
 * Make sure dpsl, dpsm, and psfc are the correct dimension sizes,
 * depending on whether they are 2D or 3D.
 */
  if(ndims_dpsl == 2) {
    if(dsizes_dpsl[0] != nlat || dsizes_dpsl[1] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsl' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }
    if(dsizes_dpsm[0] != nlat || dsizes_dpsm[1] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsm' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }
    if(dsizes_psfc[0] != nlat || dsizes_psfc[1] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'psfc' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }
  }
  else {
    if(dsizes_dpsl[0] != ntim || dsizes_dpsl[1] != nlat || 
       dsizes_dpsl[2] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsl' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }

    if(dsizes_dpsm[0] != ntim || dsizes_dpsm[1] != nlat || 
       dsizes_dpsm[2] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'dpsm' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }
    if(dsizes_psfc[0] != ntim || dsizes_psfc[1] != nlat || 
       dsizes_psfc[2] != nlon) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'psfc' must be dimensioned time x lat x lon or lat x lon");
      return(NhlFATAL);
    }
  }

/*
 * Test the sizes of 'hybd' and 'hybm', which must be 1D of length nlev.
 */
  if(dsizes_hybd[0] != nlev || dsizes_hybm[0] != nlev) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'hybd' and 'hybm' must be of length 'nlev'");
    return(NhlFATAL);
  }

/*
 * Make sure nprlev is greater than 0.
 */
  if(*nprlev < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: 'nprlev' must be greater than zero");
    return(NhlFATAL);
  }

/*
 * Coerce (first subset) of input to double, if necessary.
 */
  nlatlon    = nlat * nlon; 
  nlevlatlon = nlatlon * nlev;

  tmp_u    = coerce_input_double(u,type_u,nlevlatlon,0,NULL,NULL);
  tmp_v    = coerce_input_double(v,type_v,nlevlatlon,0,NULL,NULL);
  tmp_div =  coerce_input_double(div,type_div,nlevlatlon,0,NULL,NULL);
  tmp_pmid = coerce_input_double(pmid,type_pmid,nlevlatlon,0,NULL,NULL);
  tmp_pdel = coerce_input_double(pdel,type_pdel,nlevlatlon,0,NULL,NULL);

  tmp_dpsl = coerce_input_double(dpsl,type_dpsl,nlatlon,0,NULL,NULL);
  tmp_dpsm = coerce_input_double(dpsm,type_dpsm,nlatlon,0,NULL,NULL);
  tmp_psfc = coerce_input_double(psfc,type_psfc,nlatlon,0,NULL,NULL);

  tmp_hybd = coerce_input_double(hybd,type_hybd,nlev,0,NULL,NULL);
  tmp_hybm = coerce_input_double(hybm,type_hybm,nlev,0,NULL,NULL);

  if( tmp_u    == NULL || tmp_v    == NULL || tmp_div  == NULL || 
      tmp_dpsl == NULL || tmp_dpsm == NULL || tmp_pmid == NULL ||
      tmp_pdel == NULL || tmp_psfc == NULL || tmp_hybd == NULL ||
      tmp_hybm == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: Unable to coerce input variables to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 *
 * Also, set size for output array (omega).
 */
  size_omega = nlevlatlon * ntim;

  if(type_u == NCL_double || type_v == NCL_double) {
    type_omega = NCL_double;
    omega      = (void*)calloc(size_omega,sizeof(double));
  }
  else {
    type_omega = NCL_float;
    omega      = (void*)calloc(size_omega,sizeof(float));
  }
  tmp_omega = coerce_output_double(omega,type_omega,nlevlatlon);
      
  if(omega == NULL || tmp_omega == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"omega_ccm: Unable to allocate memory for output variable");
    return(NhlFATAL);
  }
/*
 * Call the Fortran routine.
 */ 
  index_u = index_psfc = 0;

  for(nt = 0; nt < ntim; nt++) {
/*
 * The first timestep has already been coerced, so skip it.
 */
    if(nt > 0) {
      coerce_subset_input_double(u,tmp_u,index_u,type_u,
                                 nlevlatlon,0,NULL,NULL);
      coerce_subset_input_double(v,tmp_v,index_u,type_v,
                                 nlevlatlon,0,NULL,NULL);
      coerce_subset_input_double(div,tmp_div,index_u,type_div,
                                 nlevlatlon,0,NULL,NULL);
      coerce_subset_input_double(pmid,tmp_pmid,index_u,type_pmid,
                                 nlevlatlon,0,NULL,NULL);
      coerce_subset_input_double(pdel,tmp_pdel,index_u,type_pdel,
                                 nlevlatlon,0,NULL,NULL);

      coerce_subset_input_double(dpsl,tmp_dpsl,index_psfc,type_dpsl,
                                 nlatlon,0,NULL,NULL);
      coerce_subset_input_double(dpsm,tmp_dpsm,index_psfc,type_dpsm,
                                 nlatlon,0,NULL,NULL);
      coerce_subset_input_double(psfc,tmp_psfc,index_psfc,type_psfc,
                                 nlatlon,0,NULL,NULL);
    }
    if(type_omega == NCL_double) {
/*
 * Point tmp_omega to appropriate location in omega
 */
      tmp_omega = &((double*)omega)[index_u];
    }
    NGCALLF(omcalcccm,OMCALCCCM)(tmp_u, tmp_v, tmp_div, tmp_dpsl, tmp_dpsm, 
                                 tmp_pmid, tmp_pdel, tmp_psfc, tmp_hybd, 
                                 tmp_hybm, nprlev, tmp_omega, &inlon, &inlat,
                                 &inlev);
/*
 * If the output is to be float, then do the coercion here.
 */
    if(type_omega == NCL_float) {
      coerce_output_float_only(omega,tmp_omega,nlevlatlon,index_u);
    }
/*
 * Implement the pointers into the arrays.
 */
    index_u    += nlevlatlon;
    index_psfc += nlatlon;
  }
/*
 * Free memory.
 */
  if(type_u    != NCL_double) NclFree(tmp_u);
  if(type_v    != NCL_double) NclFree(tmp_v);
  if(type_div  != NCL_double) NclFree(tmp_div);
  if(type_dpsl != NCL_double) NclFree(tmp_dpsl);
  if(type_dpsm != NCL_double) NclFree(tmp_dpsm);
  if(type_pmid != NCL_double) NclFree(tmp_pmid);
  if(type_pdel != NCL_double) NclFree(tmp_pdel);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_hybd != NCL_double) NclFree(tmp_hybd);
  if(type_hybm != NCL_double) NclFree(tmp_hybm);
  if(type_omega != NCL_double) NclFree(tmp_omega);
/*
 * Set up variable to return.
 */
  return(NclReturnValue(omega,ndims_u,dsizes_u,NULL,type_omega,0));
}

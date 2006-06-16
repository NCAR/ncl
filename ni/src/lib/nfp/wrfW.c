#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcomputetk,DCOMPUTETK)(double *,double *,double *,int *);
extern void NGCALLF(dcomputerh,DCOMPUTERH)(double *,double *,double *,
                                           double *,int *);
extern void NGCALLF(dcomputeseaprs,DCOMPUTESEAPRS)(int *,int *,int *,
                                                   double *,double *,
                                                   double *,double *,
                                                   double *,double *,
                                                   double *,double *);
extern void NGCALLF(dinterp3dz,DINTERP3DZ)(double *,double *,double *,
                                           double *,int *,int *, int*);


NhlErrorTypes wrf_compute_temperature_W( void )
{
/*
 * Input array variables
 */
  void *p, *theta;
  string *units;
  double *tmp_p, *tmp_theta;
  int ndims_p, ndims_theta;
  int dsizes_p[NCL_MAX_DIMENSIONS], dsizes_theta[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_theta;

/*
 * Output variable.
 */
  void *t;
  double *tmp_t;
  int size_t;
  NclBasicDataTypes type_t;
/*
 * Various
 */
  int i, nx, size_leftmost, index_p;
  char *units_char;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
           0,
           3,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           2);

  theta = (void*)NclGetArgValue(
           1,
           3,
           &ndims_theta,
           dsizes_theta,
           NULL,
           NULL,
           &type_theta,
           2);

  units = (string*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * Convert to a char* so we can check it.
 */
  units_char = NrmQuarkToString(*units);
  if (strcmp(units_char, "c") && strcmp(units_char, "C")) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: invalid units; must be 'c', 'f', or 'k'");
    return(NhlFATAL);
  }

/*
 * Error checking. Input variables must be same size.
 */
  if(ndims_p != ndims_theta) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: The p and theta arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_theta[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: p and theta must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_p-1; i++) size_leftmost *= dsizes_p[i];
  nx = dsizes_p[ndims_p-1];
  size_t = size_leftmost * nx;

/* 
 * Allocate space for coercing input arrays.  If the input p or theta
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the two input arrays
 * are double.
 */
  type_t = NCL_float;
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(nx,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_t = NCL_double;
  }

  if(type_theta != NCL_double) {
    tmp_theta = (double *)calloc(nx,sizeof(double));
    if(tmp_theta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_t = NCL_double;
  }

/*
 * Allocate space for output array.
 */ 
  if(type_t == NCL_double) {
    t = (double *)calloc(size_t,sizeof(double));
    if(t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    t     = (float *)calloc(size_t,sizeof(float));
    tmp_t = (double *)calloc(nx,sizeof(double));
    if(tmp_t == NULL || t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_temperature: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */
  index_p = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of p (tmp_p) to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_p,type_p,nx,0,NULL,NULL);
    }
    else {
      tmp_p = &((double*)p)[index_p];
    }
/*
 * Coerce subsection of theta (tmp_theta) to double if ncessary.
 */
    if(type_theta != NCL_double) {
      coerce_subset_input_double(theta,tmp_theta,index_p,type_theta,nx,
                                 0,NULL,NULL);
    }
    else {
      tmp_theta = &((double*)theta)[index_p];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_t == NCL_double) tmp_t = &((double*)t)[index_p];
/*
 * Call Fortran routine.
 */
    NGCALLF(dcomputetk,DCOMPUTETK)(tmp_t,tmp_p,tmp_theta,&nx);

/*
 * Coerce output back to float if necessary.
 */
    if(type_t == NCL_float) {
      coerce_output_float_only(t,tmp_t,nx,index_p);
    }

    index_p += nx;    /* Increment index */
  }
/*
 * Free up memory.
 */
  if(type_p     != NCL_double) NclFree(tmp_p);
  if(type_theta != NCL_double) NclFree(tmp_theta);
  if(type_t     != NCL_double) NclFree(tmp_t);

  return(NclReturnValue(t,ndims_p,dsizes_p,NULL,type_t,0));
}

NhlErrorTypes wrf_compute_rh_W( void )
{
/*
 * Input array variables
 */
  void *qv, *p, *t;
  double *tmp_qv, *tmp_p, *tmp_t;
  int ndims_qv, ndims_p, ndims_t;
  int dsizes_qv[NCL_MAX_DIMENSIONS], dsizes_p[NCL_MAX_DIMENSIONS];
  int dsizes_t[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_qv, type_p, type_t;

/*
 * Output variable.
 */
  void *rh;
  double *tmp_rh;
  int size_rh;
  NclBasicDataTypes type_rh;
/*
 * Various
 */
  int i, nx, size_leftmost, index_qv;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  qv = (void*)NclGetArgValue(
           0,
           3,
           &ndims_qv,
           dsizes_qv,
           NULL,
           NULL,
           &type_qv,
           2);

  p = (void*)NclGetArgValue(
           1,
           3,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           2);

  t = (void*)NclGetArgValue(
           2,
           3,
           &ndims_t,
           dsizes_t,
           NULL,
           NULL,
           &type_t,
           2);

/*
 * Error checking. Input variables must be same size.
 */
  if(ndims_qv != ndims_t || ndims_p != ndims_t) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: The qv, p, and t arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_qv; i++) {
    if(dsizes_qv[i] != dsizes_t[i] || dsizes_p[i] != dsizes_t[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: qv, p, and t must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_qv-1; i++) size_leftmost *= dsizes_qv[i];
  nx = dsizes_qv[ndims_qv-1];
  size_rh = size_leftmost * nx;

/* 
 * Allocate space for coercing input arrays.  If the input qv, p, or t
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the two input arrays
 * are double.
 */
  type_rh = NCL_float;
  if(type_qv != NCL_double) {
    tmp_qv = (double *)calloc(nx,sizeof(double));
    if(tmp_qv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(nx,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(nx,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }

/*
 * Allocate space for output array.
 */ 
  if(type_rh == NCL_double) {
    rh = (double *)calloc(size_rh,sizeof(double));
    if(rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    rh     = (float *)calloc(size_rh,sizeof(float));
    tmp_rh = (double *)calloc(nx,sizeof(double));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_rh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */
  index_qv = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of qv (tmp_qv) to double if necessary.
 */
    if(type_qv != NCL_double) {
      coerce_subset_input_double(qv,tmp_qv,index_qv,type_qv,nx,0,NULL,NULL);
    }
    else {
      tmp_qv = &((double*)qv)[index_qv];
    }
/*
 * Coerce subsection of p (tmp_p) to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_qv,type_p,nx,0,NULL,NULL);
    }
    else {
      tmp_p = &((double*)p)[index_qv];
    }
/*
 * Coerce subsection of t (tmp_t) to double if ncessary.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_qv,type_t,nx,
                                 0,NULL,NULL);
    }
    else {
      tmp_t = &((double*)t)[index_qv];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_rh == NCL_double) tmp_rh = &((double*)rh)[index_qv];
/*
 * Call Fortran routine.
 */
    NGCALLF(dcomputerh,DCOMPUTERH)(tmp_qv,tmp_p,tmp_t,tmp_rh,&nx);

/*
 * Coerce output back to float if necessary.
 */
    if(type_rh == NCL_float) {
      coerce_output_float_only(rh,tmp_rh,nx,index_qv);
    }

    index_qv += nx;    /* Increment index */
  }
/*
 * Free up memory.
 */
  if(type_qv != NCL_double) NclFree(tmp_qv);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_rh != NCL_double) NclFree(tmp_rh);

  return(NclReturnValue(rh,ndims_qv,dsizes_qv,NULL,type_rh,0));
}

NhlErrorTypes wrf_compute_slp_W( void )
{
/*
 * Input array variables
 */
  void *z, *t, *p, *q;
  double *tmp_z, *tmp_t, *tmp_p, *tmp_q;
  int ndims_z, ndims_t, ndims_p, ndims_q;
  int dsizes_z[NCL_MAX_DIMENSIONS], dsizes_t[NCL_MAX_DIMENSIONS];
  int dsizes_p[NCL_MAX_DIMENSIONS], dsizes_q[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_z, type_t, type_p, type_q;

/*
 * Output variable.
 */
  void *slp;
  double *tmp_slp;
  int size_slp;
  NclBasicDataTypes type_slp;
/*
 * Various
 */
  int i, nx, ny, nz, nxy, nxyz, size_leftmost, index_z;
  double *tmp_slv, *tmp_srf, *tmp_lvl;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           4,
           &ndims_z,
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           2);

  t = (void*)NclGetArgValue(
           1,
           4,
           &ndims_t,
           dsizes_t,
           NULL,
           NULL,
           &type_t,
           2);

  p = (void*)NclGetArgValue(
           2,
           4,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           2);

  q = (void*)NclGetArgValue(
           3,
           4,
           &ndims_q,
           dsizes_q,
           NULL,
           NULL,
           &type_q,
           2);

/*
 * Error checking. Input variables must be same size, and must have at least
 * 3 dimensions.
 */
  if(ndims_z != ndims_t || ndims_z != ndims_p || ndims_z != ndims_q) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: The z, t, p, and q arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  if(ndims_z < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: The z, t, p, and q arrays must have at least 3 dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_z; i++) {
    if(dsizes_z[i] != dsizes_t[i] || dsizes_z[i] != dsizes_p[i] ||
       dsizes_z[i] != dsizes_q[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: z, t, p, and q must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_z-3; i++) size_leftmost *= dsizes_z[i];
  nx = dsizes_z[ndims_z-1];
  ny = dsizes_z[ndims_z-2];
  nz = dsizes_z[ndims_z-3];
  nxy  = nx * ny;
  nxyz = nxy * nz;
  size_slp = size_leftmost * nxyz;

/* 
 * Allocate space for coercing input arrays.  If the input q, p, or t
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the two input arrays
 * are double.
 */
  type_slp = NCL_float;

  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(nxyz,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
     type_slp = NCL_double;
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(nxyz,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
  }

  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(nxyz,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
  }

  if(type_q != NCL_double) {
    tmp_q = (double *)calloc(nxyz,sizeof(double));
    if(tmp_q == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
  }

/*
 * Allocate space for work arrays.
 */ 
  tmp_slv = (double *)calloc(nxy,sizeof(double));
  tmp_srf = (double *)calloc(nxy,sizeof(double));
  tmp_lvl = (double *)calloc(nxy,sizeof(double));
  if(tmp_slv == NULL || tmp_srf == NULL || tmp_lvl == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */ 
  if(type_slp == NCL_double) {
    slp = (double *)calloc(size_slp,sizeof(double));
    if(slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    slp     = (float *)calloc(size_slp,sizeof(float));
    tmp_slp = (double *)calloc(nxyz,sizeof(double));
    if(tmp_slp == NULL || slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_compute_slp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Loop across leftmost dimensions and call the Fortran routine
 * for each three-dimensional subsection.
 */
  index_z = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of z (tmp_z) to double if necessary.
 */
    if(type_z != NCL_double) {
      coerce_subset_input_double(z,tmp_z,index_z,type_z,nxyz,0,NULL,NULL);
    }
    else {
      tmp_z = &((double*)z)[index_z];
    }
/*
 * Coerce subsection of p (tmp_p) to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_z,type_p,nxyz,0,NULL,NULL);
    }
    else {
      tmp_p = &((double*)p)[index_z];
    }
/*
 * Coerce subsection of t (tmp_t) to double if ncessary.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_z,type_t,nxyz,0,NULL,NULL);
    }
    else {
      tmp_t = &((double*)t)[index_z];
    }

/*
 * Coerce subsection of q (tmp_q) to double if necessary.
 */
    if(type_q != NCL_double) {
      coerce_subset_input_double(q,tmp_q,index_z,type_q,nx,0,NULL,NULL);
    }
    else {
      tmp_q = &((double*)q)[index_z];
    }
/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_slp == NCL_double) tmp_slp = &((double*)slp)[index_z];
/*
 * Call Fortran routine.
 */
    NGCALLF(dcomputeseaprs,DCOMPUTESEAPRS)(&nx,&ny,&nz,tmp_z,tmp_t,tmp_p,
                                           tmp_q,tmp_slp,tmp_slv,tmp_srf,
                                           tmp_lvl);
/*
 * Coerce output back to float if necessary.
 */
    if(type_slp == NCL_float) {
      coerce_output_float_only(slp,tmp_slp,nx,index_z);
    }

    index_z += nxyz;    /* Increment index */
  }
/*
 * Free up memory.
 */
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_slp != NCL_double) NclFree(tmp_slp);
  NclFree(tmp_slv);
  NclFree(tmp_srf);
  NclFree(tmp_lvl);

  return(NclReturnValue(slp,ndims_z,dsizes_z,NULL,type_slp,0));
}

NhlErrorTypes wrf_interp_3dz_W( void )
{
/*
 * Input array variables
 */
  void *v3d, *z, *loc;
  double *tmp_v3d, *tmp_z, *tmp_loc;
  int ndims_v3d, ndims_z, ndims_loc;
  int dsizes_v3d[NCL_MAX_DIMENSIONS], dsizes_z[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_v3d, type_z, type_loc;

/*
 * Output variable.
 */
  void *v2d;
  double *tmp_v2d;
  int ndims_v2d, *dsizes_v2d, size_v2d;
  NclBasicDataTypes type_v2d;
/*
 * Various
 */
  int i, nx, ny, nz, nxy, nxyz, size_leftmost, index_v3d, index_v2d;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  v3d = (void*)NclGetArgValue(
           0,
           3,
           &ndims_v3d,
           dsizes_v3d,
           NULL,
           NULL,
           &type_v3d,
           2);

  z = (void*)NclGetArgValue(
           1,
           3,
           &ndims_z,
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           2);

  loc = (void*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_loc,
           2);

/*
 * Error checking. First two input variables must be same size.
 */
  if(ndims_v3d != ndims_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: The v3d and z arrays must have the same number of dimensions");
    return(NhlFATAL);
  }

  if(ndims_v3d < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: The v3d and z arrays must have at least 3 dimensions");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_v3d; i++) {
    if(dsizes_v3d[i] != dsizes_z[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: v3d and z must be the same dimensionality");
      return(NhlFATAL);
    }
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_v3d-3; i++) size_leftmost *= dsizes_v3d[i];
  nx = dsizes_v3d[ndims_v3d-1];
  ny = dsizes_v3d[ndims_v3d-2];
  nz = dsizes_v3d[ndims_v3d-3];
  nxy = nx * ny;
  nxyz = nxy * nz;
  size_v2d = size_leftmost * nxy;

/* 
 * Allocate space for coercing input arrays.  If the input v3d or z
 * are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the two input arrays
 * are double.
 */
  type_v2d = NCL_float;
  if(type_v3d != NCL_double) {
    tmp_v3d = (double *)calloc(nxyz,sizeof(double));
    if(tmp_v3d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v2d = NCL_double;
  }
  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(nxyz,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v2d = NCL_double;
  }
/*
 * Coerce loc (tmp_loc) to double if ncessary.
 */
  tmp_loc = coerce_input_double(loc,type_loc,1,0,NULL,NULL);

/*
 * Allocate space for output array.
 */ 
  if(type_v2d == NCL_double) {
    v2d = (double *)calloc(size_v2d,sizeof(double));
    if(v2d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    v2d     = (float *)calloc(size_v2d,sizeof(float));
    tmp_v2d = (double *)calloc(nxy,sizeof(double));
    if(tmp_v2d == NULL || v2d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Set dimension sizes for output array.
 */
  ndims_v2d = ndims_v3d;     /* 4 x dimsizes(v3d)-1 */
  dsizes_v2d = (int*)calloc(ndims_v2d,sizeof(int));  
  if( dsizes_v2d == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3dz: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_v2d[0] = 4;
  for(i = 1; i <= ndims_v3d-1; i++) dsizes_v2d[i] = dsizes_v3d[i];
/*
 * Loop across leftmost dimensions and call the Fortran routine
 * for reach three-dimensional subsection.
 */
  index_v2d = index_v3d = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of v3d (tmp_v3d) to double if necessary.
 */
    if(type_v3d != NCL_double) {
      coerce_subset_input_double(v3d,tmp_v3d,index_v3d,type_v3d,nxyz,
                                 0,NULL,NULL);
    }
    else {
      tmp_v3d = &((double*)v3d)[index_v3d];
    }
/*
 * Coerce subsection of z (tmp_z) to double if necessary.
 */
    if(type_z != NCL_double) {
      coerce_subset_input_double(z,tmp_z,index_v3d,type_z,nxyz,0,NULL,NULL);
    }
    else {
      tmp_z = &((double*)z)[index_v3d];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_v2d == NCL_double) tmp_v2d = &((double*)v2d)[index_v2d];
/*
 * Call Fortran routine.
 */
    NGCALLF(dinterp3dz,DINTERP3DZ)(tmp_v3d,tmp_v2d,tmp_z,tmp_loc,
                                   &nx,&ny,&nz);

/*
 * Coerce output back to float if necessary.
 */
    if(type_v2d == NCL_float) {
      coerce_output_float_only(v2d,tmp_v2d,nxyz,index_v2d);
    }

    index_v3d += nxyz;    /* Increment index */
    index_v2d += nxy;     /* Increment index */
  }
/*
 * Free up memory.
 */
  if(type_v3d != NCL_double) NclFree(tmp_v3d);
  if(type_z   != NCL_double) NclFree(tmp_z);
  if(type_loc != NCL_double) NclFree(tmp_loc);
  if(type_v2d != NCL_double) NclFree(tmp_v2d);

  return(NclReturnValue(v2d,ndims_v2d,dsizes_v2d,NULL,type_v2d,0));
}


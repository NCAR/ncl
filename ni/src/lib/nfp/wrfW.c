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

extern void NGCALLF(dinterp2dxy,DINTERP2DXY)(double *,double *,double *,
                                             int *,int *,int *, int*);

extern void NGCALLF(dbint3d,DBINT3D)(double *,double *,double *, double *,
                                     int *, int *, int *, int *,
                                     int *, int *, int *);


extern void NGCALLF(dmaptform,DMAPTFORM)(double *,int *,int *, int *, double *,
                                         double *,double *,double *,double *,
                                         double *,double *,double *,int *);

NhlErrorTypes wrf_tk_W( void )
{
/*
 * Input array variables
 */
  void *p, *theta;
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

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
           0,
           2,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           2);

  theta = (void*)NclGetArgValue(
           1,
           2,
           &ndims_theta,
           dsizes_theta,
           NULL,
           NULL,
           &type_theta,
           2);

/*
 * Error checking. Input variables must be same size.
 */
  if(ndims_p != ndims_theta) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: The p and theta arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_theta[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: p and theta must be the same dimensionality");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_t = NCL_double;
  }

  if(type_theta != NCL_double) {
    tmp_theta = (double *)calloc(nx,sizeof(double));
    if(tmp_theta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: Unable to allocate memory for coercing input array to double");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    t     = (float *)calloc(size_t,sizeof(float));
    tmp_t = (double *)calloc(nx,sizeof(double));
    if(tmp_t == NULL || t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_tk: Unable to allocate memory for output array");
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

NhlErrorTypes wrf_rh_W( void )
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: The qv, p, and t arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_qv; i++) {
    if(dsizes_qv[i] != dsizes_t[i] || dsizes_p[i] != dsizes_t[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: qv, p, and t must be the same dimensionality");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(nx,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_rh = NCL_double;
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(nx,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: Unable to allocate memory for coercing input array to double");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    rh     = (float *)calloc(size_rh,sizeof(float));
    tmp_rh = (double *)calloc(nx,sizeof(double));
    if(tmp_rh == NULL || rh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_rh: Unable to allocate memory for output array");
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

NhlErrorTypes wrf_slp_W( void )
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: The z, t, p, and q arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  if(ndims_z < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: The z, t, p, and q arrays must have at least 3 dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_z; i++) {
    if(dsizes_z[i] != dsizes_t[i] || dsizes_z[i] != dsizes_p[i] ||
       dsizes_z[i] != dsizes_q[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: z, t, p, and q must be the same dimensionality");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
     type_slp = NCL_double;
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(nxyz,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
  }

  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(nxyz,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_slp = NCL_double;
  }

  if(type_q != NCL_double) {
    tmp_q = (double *)calloc(nxyz,sizeof(double));
    if(tmp_q == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for coercing input array to double");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */ 
  if(type_slp == NCL_double) {
    slp = (double *)calloc(size_slp,sizeof(double));
    if(slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    slp     = (float *)calloc(size_slp,sizeof(float));
    tmp_slp = (double *)calloc(nxyz,sizeof(double));
    if(tmp_slp == NULL || slp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_slp: Unable to allocate memory for output array");
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

NhlErrorTypes wrf_interp_3d_z_W( void )
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
  if(ndims_v3d < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: The v3d and z arrays must have at least 3 dimensions");
    return(NhlFATAL);
  }

  if(ndims_v3d != ndims_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: The v3d and z arrays must have the same number of dimensions");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_v3d; i++) {
    if(dsizes_v3d[i] != dsizes_z[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: v3d and z must be the same dimensionality");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v2d = NCL_double;
  }
  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(nxyz,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: Unable to allocate memory for coercing input array to double");
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    v2d     = (float *)calloc(size_v2d,sizeof(float));
    tmp_v2d = (double *)calloc(nxy,sizeof(double));
    if(tmp_v2d == NULL || v2d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Set dimension sizes for output array.
 */
  ndims_v2d = ndims_v3d-1;
  dsizes_v2d = (int*)calloc(ndims_v2d,sizeof(int));  
  if( dsizes_v2d == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_3d_z: Unable to allocate memory for holding dimension sizes");
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

    index_v3d += nxyz;
    index_v2d += nxy;
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


NhlErrorTypes wrf_interp_2d_xy_W( void )
{
/*
 * Input array variables
 */
  void *v3d, *xy;
  double *tmp_v3d, *tmp_xy;
  int ndims_v3d, ndims_xy;
  int dsizes_v3d[NCL_MAX_DIMENSIONS], dsizes_xy[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_v3d, type_xy;

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
  int i, nx, ny, nz, nxy, nxyz, nxy2, size_leftmost;
  int index_v3d, index_v2d, index_xy;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  v3d = (void*)NclGetArgValue(
           0,
           2,
           &ndims_v3d,
           dsizes_v3d,
           NULL,
           NULL,
           &type_v3d,
           2);

  xy = (void*)NclGetArgValue(
           1,
           2,
           &ndims_xy,
           dsizes_xy,
           NULL,
           NULL,
           &type_xy,
           2);

/*
 * Error checking.
 */
  if(ndims_v3d < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: The v3d array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(ndims_v3d != (ndims_xy+1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: The v3d array must have one more dimension than the xy array");
    return(NhlFATAL);
  }
  if(dsizes_xy[ndims_xy-1] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: The rightmost dimension of xy must be 2");
    return(NhlFATAL);
  }
  nz  = dsizes_v3d[ndims_v3d-3];
  ny  = dsizes_v3d[ndims_v3d-2];
  nx  = dsizes_v3d[ndims_v3d-1];
  nxy = dsizes_xy[ndims_xy-2];

  if(nxy != (nx * ny)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: The second-to-the-last dimension of xy must be nx * ny");
    return(NhlFATAL);
  }
/*
 * Check leftmost dimensions, if any, and calculate their size.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_v3d-3; i++) {
    if(dsizes_v3d[i] != dsizes_xy[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: The leftmost dimensions of v3d and xy must be the same");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_v3d[i];
  }
  nxyz = nxy * nz;
  nxy2 = 2 * nxy;

  size_v2d = size_leftmost * nxy * nz;

/* 
 * Allocate space for coercing input arrays.  If the input v3d or xy
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v2d = NCL_double;
  }
  if(type_xy != NCL_double) {
    tmp_xy = (double *)calloc(nxy2,sizeof(double));
    if(tmp_xy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v2d = NCL_double;
  }

/*
 * Allocate space for output array.
 */ 
  if(type_v2d == NCL_double) {
    v2d = (double *)calloc(size_v2d,sizeof(double));
    if(v2d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    v2d     = (float *)calloc(size_v2d,sizeof(float));
    tmp_v2d = (double *)calloc(nxyz,sizeof(double));
    if(tmp_v2d == NULL || v2d == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Set dimension sizes for output array.
 */
  ndims_v2d = ndims_xy;     /* leftmost dims x nz x nxy */
  dsizes_v2d = (int*)calloc(ndims_v2d,sizeof(int));  
  if( dsizes_v2d == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_2d_xy: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 1; i < ndims_v2d-2; i++) dsizes_v2d[i] = dsizes_xy[i];
  dsizes_v2d[ndims_v2d-2] = nz;
  dsizes_v2d[ndims_v2d-1] = nxy;

/*
 * Loop across leftmost dimensions and call the Fortran routine
 * for reach three-dimensional subsection.
 */
  index_v2d = index_v3d = index_xy = 0;
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
 * Coerce subsection of xy (tmp_xy) to double if necessary.
 */
    if(type_xy != NCL_double) {
      coerce_subset_input_double(xy,tmp_xy,index_xy,type_xy,nxy2,0,NULL,NULL);
    }
    else {
      tmp_xy = &((double*)xy)[index_xy];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_v2d == NCL_double) tmp_v2d = &((double*)v2d)[index_v2d];
/*
 * Call Fortran routine.
 */
    NGCALLF(dinterp2dxy,DINTERP2DXY)(tmp_v3d,tmp_v2d,tmp_xy,&nx,&ny,&nz,&nxy);

/*
 * Coerce output back to float if necessary.
 */
    if(type_v2d == NCL_float) {
      coerce_output_float_only(v2d,tmp_v2d,nxyz,index_v2d);
    }

    index_v3d += nxyz;    /* Increment index */
    index_xy  += nxy2;
    index_v2d += nxy;
  }
/*
 * Free up memory.
 */
  if(type_v3d != NCL_double) NclFree(tmp_v3d);
  if(type_xy  != NCL_double) NclFree(tmp_xy);
  if(type_v2d != NCL_double) NclFree(tmp_v2d);

  return(NclReturnValue(v2d,ndims_v2d,dsizes_v2d,NULL,type_v2d,0));
}


NhlErrorTypes wrf_interp_1d_W( void )
{
/*
 * Input array variables
 */
  void *v_in, *z_in, *z_out;
  double *tmp_v_in, *tmp_z_in, *tmp_z_out;
  int ndims_v_in, ndims_z_in, ndims_z_out;
  int dsizes_v_in[NCL_MAX_DIMENSIONS], dsizes_z_in[NCL_MAX_DIMENSIONS];
  int dsizes_z_out[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_v_in, type_z_in, type_z_out;

/*
 * Output variable.
 */
  void *v_out;
  double *tmp_v_out;
  int *dsizes_v_out, size_v_out;
  NclBasicDataTypes type_v_out;
/*
 * Various
 */
  int i, nz_in, nz_out, size_leftmost, index_v_in, index_v_out;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  v_in = (void*)NclGetArgValue(
           0,
           3,
           &ndims_v_in,
           dsizes_v_in,
           NULL,
           NULL,
           &type_v_in,
           2);

  z_in = (void*)NclGetArgValue(
           1,
           3,
           &ndims_z_in,
           dsizes_z_in,
           NULL,
           NULL,
           &type_z_in,
           2);

  z_out = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z_out,
           dsizes_z_out,
           NULL,
           NULL,
           &type_z_out,
           2);

/*
 * Error checking.
 */
  if(ndims_v_in != ndims_z_in || ndims_v_in != ndims_z_out) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: The v_in, z_in, and z_out arrays must be the same number of dimensions");
    return(NhlFATAL);
  }
  nz_in  = dsizes_v_in[ndims_v_in-1];
  nz_out = dsizes_z_out[ndims_z_out-1];
  if(dsizes_z_in[ndims_z_in-1] != nz_in) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: The rightmost dimesion of v_in and z_in must be the same");
    return(NhlFATAL);
  }
/*
 * Calculate leftmost dimensions, if any and check their sizes.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_v_in-1; i++ ) {
    if(dsizes_v_in[ndims_z_in-1] != dsizes_z_in[i] || 
       dsizes_v_in[ndims_z_in-1] != dsizes_z_out[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: The input arrays must be the same dimensionality");
      return(NhlFATAL);
    }
    if(i != (ndims_v_in-1)) size_leftmost *= dsizes_v_in[i];
  }

  size_v_out = size_leftmost * nz_out;

/* 
 * Allocate space for coercing input arrays.  If the input arrays
 * are already double, then we don't need to allocate space for the
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the two input arrays
 * are double.
 */
  type_v_out = NCL_float;
  if(type_v_in != NCL_double) {
    tmp_v_in = (double *)calloc(nz_in,sizeof(double));
    if(tmp_v_in == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v_out = NCL_double;
  }
  if(type_z_in != NCL_double) {
    tmp_z_in = (double *)calloc(nz_in,sizeof(double));
    if(tmp_z_in == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v_out = NCL_double;
  }

  if(type_z_out != NCL_double) {
    tmp_z_out = (double *)calloc(nz_out,sizeof(double));
    if(tmp_z_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_v_out = NCL_double;
  }

/*
 * Allocate space for output array.
 */ 
  if(type_v_out == NCL_double) {
    v_out = (double *)calloc(size_v_out,sizeof(double));
    if(v_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    v_out     = (float *)calloc(size_v_out,sizeof(float));
    tmp_v_out = (double *)calloc(nz_out,sizeof(double));
    if(tmp_v_out == NULL || v_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Set dimension sizes for output array.
 */
  dsizes_v_out = (int*)calloc(ndims_z_out,sizeof(int));  
  if( dsizes_v_out == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_interp_1d: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 1; i < ndims_z_out; i++) dsizes_v_out[i] = dsizes_z_out[i];

/*
 * Loop across leftmost dimensions and call the Fortran routine
 * for reach one-dimensional subsection.
 */
  index_v_out = index_v_in = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of v_in (tmp_v_in) to double if necessary.
 */
    if(type_v_in != NCL_double) {
      coerce_subset_input_double(v_in,tmp_v_in,index_v_in,type_v_in,nz_in,
                                 0,NULL,NULL);
    }
    else {
      tmp_v_in = &((double*)v_in)[index_v_in];
    }
/*
 * Coerce subsection of z_in (tmp_z_in) to double if necessary.
 */
    if(type_z_in != NCL_double) {
      coerce_subset_input_double(z_in,tmp_z_in,index_v_in,type_z_in,nz_in,0,NULL,NULL);
    }
    else {
      tmp_z_in = &((double*)z_in)[index_v_in];
    }

/*
 * Coerce subsection of z_out (tmp_z_out) to double if necessary.
 */
    if(type_z_out != NCL_double) {
      coerce_subset_output_double(z_out,tmp_z_out,index_v_out,type_z_out,nz_out,0,NULL,NULL);
    }
    else {
      tmp_z_out = &((double*)z_out)[index_v_out];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_v_out == NCL_double) tmp_v_out = &((double*)v_out)[index_v_out];
/*
 * Call Fortran routine.
 */
    NGCALLF(dinterp1d,DINTERP1D)(tmp_v_in,tmp_v_out,tmp_z_in,tmp_z_out,&nz_in,
                                 &nz_out);

/*
 * Coerce output back to float if necessary.
 */
    if(type_v_out == NCL_float) {
      coerce_output_float_only(v_out,tmp_v_out,nz_out,index_v_out);
    }

    index_v_in  += nz_in;
    index_v_out += nz_out;
  }
/*
 * Free up memory.
 */
  if(type_v_in != NCL_double) NclFree(tmp_v_in);
  if(type_z_in  != NCL_double) NclFree(tmp_z_in);
  if(type_z_out != NCL_double) NclFree(tmp_z_out);
  if(type_v_out != NCL_double) NclFree(tmp_v_out);

  return(NclReturnValue(v_out,ndims_z_out,dsizes_v_out,NULL,type_v_out,0));
}

NhlErrorTypes wrf_bint_W( void )
{
/*
 * Input array variables
 */
  void *data_in, *obsii, *obsjj;
  double *tmp_data_in, *tmp_obsii, *tmp_obsjj;
  int *icrs, *jcrs;
  int ndims_data_in, ndims_obsii, ndims_obsjj;
  int dsizes_data_in[NCL_MAX_DIMENSIONS]; 
  int dsizes_obsii[NCL_MAX_DIMENSIONS], dsizes_obsjj[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_data_in, type_obsii, type_obsjj;
  
/*
 * Output variable.
 */
  void *data_out;
  double *tmp_data_out;
  int *dsizes_data_out, size_data_out;
  NclBasicDataTypes type_data_out;
/*
 * Various
 */
  int i, nx, ny, nz, nobsicrs, nobsjcrs, size_leftmost; 
  int nxyz, nobsij, nobsijz, index_data_in, index_data_out, index_nobsij;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  data_in = (void*)NclGetArgValue(
           0,
           5,
           &ndims_data_in,
           dsizes_data_in,
           NULL,
           NULL,
           &type_data_in,
           2);

  obsii = (void*)NclGetArgValue(
           1,
           5,
           &ndims_obsii,
           dsizes_obsii,
           NULL,
           NULL,
           &type_obsii,
           2);

  obsjj = (void*)NclGetArgValue(
           2,
           5,
           &ndims_obsjj,
           dsizes_obsjj,
           NULL,
           NULL,
           &type_obsjj,
           2);

  icrs = (int*)NclGetArgValue(
           3,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

  jcrs = (int*)NclGetArgValue(
           4,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

/*
 * Error checking.
 */
  if(ndims_data_in < 2 || ndims_obsii < 2 || ndims_obsjj < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: The data_in, obsii, and obsjj arrays must have at least two dimensions");
    return(NhlFATAL);
  }
  if(ndims_obsii != ndims_obsjj) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: The obsii and obsjj arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  if((ndims_data_in == 2 && ndims_obsii != 2) || 
     (ndims_data_in  > 2 && ndims_data_in != (ndims_obsii+1))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: The data_in, obsii, and obsjj arrays must all be two-dimensional, or data_in must be greater than two dimensions and have one more dimension than obsii and obsjj");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_obsii; i++) {
    if(dsizes_obsii[i] != dsizes_obsjj[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: The obsii and obsjj arrays must be the same dimension sizes");
      return(NhlFATAL);
    }
  }

/*
 * If data_in is greater than 3 dimensions, then check that these
 * extra dimensions are all the same length in the three input 
 * arrays.
 *
 * While we're here, calculate the size of the leftmost dimensions.
 */
  size_leftmost = 1;
  if(ndims_data_in > 3) {
    for(i = 0; i < ndims_data_in-3; i++) {
      if(dsizes_data_in[i] != dsizes_obsii[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: The rightmost dimensions of data_in, obsii, obsjj must be the same");
        return(NhlFATAL);
      }
      size_leftmost *= dsizes_data_in[i];
    }
  }

/*
 * Store some dimension sizes and output data array sizes.
 */
  nx = dsizes_data_in[ndims_data_in-1];
  ny = dsizes_data_in[ndims_data_in-2];
  if(ndims_data_in > 2) {
    nz = dsizes_data_in[ndims_data_in-3];
  }
  else {
    nz = 1;
  }
  nobsicrs = dsizes_obsii[ndims_obsii-1];
  nobsjcrs = dsizes_obsii[ndims_obsii-2];
  nxyz     = nx * ny * nz;
  nobsij   = nobsicrs * nobsjcrs;
  nobsijz  = nobsij * nz;

  size_data_out = size_leftmost * nobsijz;

/* 
 * Allocate space for coercing input arrays.  If the input data_in, obsii,
 * or obsjj are already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless any of the input arrays
 * are double.
 */
  type_data_out = NCL_float;
  if(type_data_in != NCL_double) {
    tmp_data_in = (double *)calloc(nxyz,sizeof(double));
    if(tmp_data_in == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_data_out = NCL_double;
  }

  if(type_obsii != NCL_double) {
    tmp_obsii = (double *)calloc(nobsij,sizeof(double));
    if(tmp_obsii == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_data_out = NCL_double;
  }

  if(type_obsjj != NCL_double) {
    tmp_obsjj = (double *)calloc(nobsij,sizeof(double));
    if(tmp_obsjj == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_data_out = NCL_double;
  }

/*
 * Allocate space for output array.
 */ 
  if(type_data_out == NCL_double) {
    data_out = (double *)calloc(size_data_out,sizeof(double));
    if(data_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    data_out     = (float *)calloc(size_data_out,sizeof(float));
    tmp_data_out = (double *)calloc(nobsijz,sizeof(double));
    if(tmp_data_out == NULL || data_out == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Create dimension sizes for output array.
 */
  dsizes_data_out = (int*)calloc(ndims_data_in,sizeof(int));  
  if( dsizes_data_out == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wrf_bint: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 1; i < ndims_data_in-2; i++) dsizes_data_out[i] = dsizes_data_in[i];
  dsizes_data_out[ndims_data_in-2] = nobsjcrs;
  dsizes_data_out[ndims_data_in-1] = nobsicrs;
/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */
  index_data_in = index_data_out = index_nobsij = 0;
  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of data_in (tmp_data_in) to double if necessary.
 */
    if(type_data_in != NCL_double) {
      coerce_subset_input_double(data_in,tmp_data_in,index_data_in,
                                 type_data_in,nxyz,0,NULL,NULL);
    }
    else {
      tmp_data_in = &((double*)data_in)[index_data_in];
    }
/*
 * Coerce subsection of obsii (tmp_obsii) to double if ncessary.
 */
    if(type_obsii != NCL_double) {
      coerce_subset_input_double(obsii,tmp_obsii,index_nobsij,type_obsii,
                                 nobsij,0,NULL,NULL);
    }
    else {
      tmp_obsii = &((double*)obsii)[index_nobsij];
    }

/*
 * Coerce subsection of obsjj (tmp_obsjj) to double if ncessary.
 */
    if(type_obsjj != NCL_double) {
      coerce_subset_input_double(obsjj,tmp_obsjj,index_nobsij,type_obsjj,
                                 nobsij,0,NULL,NULL);
    }
    else {
      tmp_obsjj = &((double*)obsjj)[index_nobsij];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_data_out == NCL_double) {
      tmp_data_out = &((double*)data_out)[index_data_out];
    }
/*
 * Call Fortran routine.
 */
    NGCALLF(dbint3d,DBINT3D)(tmp_data_out,tmp_obsii,tmp_obsjj,tmp_data_in,
                             &nx,&ny,&nz,&nobsicrs,&nobsjcrs,icrs,jcrs);

/*
 * Coerce output back to float if necessary.
 */
    if(type_data_out == NCL_float) {
      coerce_output_float_only(data_out,tmp_data_out,nobsijz,index_data_out);
    }

/*
 * Increment indices.
 */
    index_data_in  += nxyz;
    index_data_out += nobsijz;
    index_nobsij   += nobsij;
  }
/*
 * Free up memory.
 */
  if(type_data_in  != NCL_double) NclFree(tmp_data_in);
  if(type_obsii    != NCL_double) NclFree(tmp_obsii);
  if(type_obsjj    != NCL_double) NclFree(tmp_obsjj);
  if(type_data_out != NCL_double) NclFree(tmp_data_out);

  return(NclReturnValue(data_out,ndims_data_in,dsizes_data_out,NULL,
                        type_data_out,0));
}

NhlErrorTypes wrf_maptform_W( void )
{
/*
 * Input array variables
 */
  void *dskmc, *xlatc, *xlonc, *riy, *rjx, *rlat, *rlon, *true1, *true2;
  double *tmp_dskmc, *tmp_xlatc, *tmp_xlonc, *tmp_riy, *tmp_rjx;
  double *tmp_rlat, *tmp_rlon, *tmp_true1, *tmp_true2;
  int *miycors, *mjxcors, *nproj, *idir;

  NclBasicDataTypes type_dskmc, type_xlatc, type_xlonc, type_riy, type_rjx;
  NclBasicDataTypes type_rlat, type_rlon, type_true1, type_true2;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dskmc = (void*)NclGetArgValue(
           0,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_dskmc,
           2);

  miycors = (int*)NclGetArgValue(
           1,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

  mjxcors = (int*)NclGetArgValue(
           2,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

  nproj = (int*)NclGetArgValue(
           3,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

  xlatc = (void*)NclGetArgValue(
           4,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xlatc,
           2);

  xlonc = (void*)NclGetArgValue(
           5,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_xlonc,
           2);

  riy = (void*)NclGetArgValue(
           6,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_riy,
           2);

  rjx = (void*)NclGetArgValue(
           7,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rjx,
           2);

  idir = (int*)NclGetArgValue(
           8,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);

  rlat = (void*)NclGetArgValue(
           9,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rlat,
           2);

  rlon = (void*)NclGetArgValue(
           10,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rlon,
           2);

  true1 = (void*)NclGetArgValue(
           11,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_true1,
           2);

  true2 = (void*)NclGetArgValue(
           12,
           13,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_true2,
           2);

/* 
 * Coerce void input arrays to double if necessary.
 */
  coerce_input_double(dskmc, type_dskmc, 1, 0, NULL, NULL);
  coerce_input_double(xlatc, type_xlatc, 1, 0, NULL, NULL);
  coerce_input_double(xlonc, type_xlonc, 1, 0, NULL, NULL);
  coerce_input_double(riy,   type_riy,   1, 0, NULL, NULL);
  coerce_input_double(rjx,   type_rjx,   1, 0, NULL, NULL);
  coerce_input_double(rlat,  type_rlat,  1, 0, NULL, NULL);
  coerce_input_double(rlon,  type_rlon,  1, 0, NULL, NULL);
  coerce_input_double(true1, type_true1, 1, 0, NULL, NULL);
  coerce_input_double(true2, type_true2, 1, 0, NULL, NULL);

/*
 * Call Fortran routine.
 */
    NGCALLF(dmaptform,DMAPTFORM)(tmp_dskmc,miycors,mjxcors,nproj,tmp_xlatc,
                                 tmp_xlonc,tmp_true1,tmp_true2,tmp_riy,
                                 tmp_rjx,tmp_rlat,tmp_rlon,idir); 


/*
 * Free up memory.
 */
  if(type_dskmc != NCL_double) NclFree(tmp_dskmc);
  if(type_xlatc != NCL_double) NclFree(tmp_xlatc);
  if(type_xlonc != NCL_double) NclFree(tmp_xlonc);
  if(  type_riy != NCL_double) NclFree(tmp_riy);
  if(  type_rjx != NCL_double) NclFree(tmp_rjx);
  if( type_rlat != NCL_double) NclFree(tmp_rlat);
  if( type_rlon != NCL_double) NclFree(tmp_rlon);
  if(type_true1 != NCL_double) NclFree(tmp_true1);
  if(type_true2 != NCL_double) NclFree(tmp_true2);

  return(NhlNOERROR);
}


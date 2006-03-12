#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dcapecalc3d,DCAPECALC3D)(double *prs, double *tmk, 
                                             double *qvp, double *ght,
                                             double *ter, double *sfp, 
                                             double *cape, double *cin, 
                                             int *miy, int *mjx, int *mkzh, 
                                             int *i3dflag, int *ter_follow);


NhlErrorTypes rip_cape_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *q, *z, *zsfc, *psfc;
  logical *i3dflag, *ter_follow;
  double *tmp_p, *tmp_t, *tmp_q, *tmp_z, *tmp_zsfc, *tmp_psfc;
  int ndims_p, ndims_t, ndims_q, ndims_z, ndims_zsfc, ndims_psfc;
  int dsizes_p[NCL_MAX_DIMENSIONS], dsizes_t[NCL_MAX_DIMENSIONS];
  int dsizes_q[NCL_MAX_DIMENSIONS], dsizes_z[NCL_MAX_DIMENSIONS];
  int dsizes_zsfc[NCL_MAX_DIMENSIONS], dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p, type_t, type_q, type_z, type_zsfc, type_psfc;

/*
 * Output array variables
 */
  void *cape;
  double *tmp_cape, *tmp_cin;
  NclBasicDataTypes type_cape;
  int ndims_cape, *dsizes_cape;
/*
 * Declare various variables for random purposes.
 */
  int i, miy, mjx, mkzh, size_leftmost, size_cape, size_output, size_zsfc;
  int index_cape, index_zsfc, index_cin;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 *
 */
  p = (void*)NclGetArgValue(
          0,
          8,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

  t = (void*)NclGetArgValue(
          1,
          8,
          &ndims_t,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          2);


  q = (void*)NclGetArgValue(
          2,
          8,
          &ndims_q,
          dsizes_q,
          NULL,
          NULL,
          &type_q,
          2);

  z = (void*)NclGetArgValue(
          3,
          8,
          &ndims_z,
          dsizes_z,
          NULL,
          NULL,
          &type_z,
          2);

  zsfc = (void*)NclGetArgValue(
          4,
          8,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          NULL,
          &type_zsfc,
          2);

  psfc = (void*)NclGetArgValue(
          5,
          8,
          &ndims_psfc,
          dsizes_psfc,
          NULL,
          NULL,
          &type_psfc,
          2);

  i3dflag = (logical*)NclGetArgValue(
          6,
          8,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  ter_follow = (logical*)NclGetArgValue(
          7,
          8,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);
  
/*
 * Check the input dimension sizes.
 */
  if(ndims_p != ndims_t || ndims_p != ndims_q || ndims_p != ndims_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: The p, t, q, and z arrays must all have the same number of dimensions");
    return(NhlFATAL);
  }

  if(ndims_zsfc != ndims_p-1 || ndims_zsfc != ndims_psfc) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: The zsfc and psfc arrays must have the same number of dimensions, and one less dimension than the other input arrays");
    return(NhlFATAL);
  }

/*
 * Now check that the dimension sizes are equal to each other.
 */
  for(i = 0; i < ndims_p; i++) {
    if(dsizes_p[i] != dsizes_t[i] || dsizes_p[i] != dsizes_q[i] || 
       dsizes_p[i] != dsizes_z[i]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: p, t, q, and z must be the same dimensionality");
    return(NhlFATAL);
    }
  }

  for(i = 0; i < ndims_psfc; i++) {
    if(dsizes_psfc[i] != dsizes_zsfc[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: psfc and zsfc must be the same dimensionality");
      return(NhlFATAL);
    }
    if(dsizes_psfc[i] != dsizes_p[i+1]) { 
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: The dimensions of psfc and zsfc must be equal to all but the lefmost dimension of the other input arrays");
      return(NhlFATAL);
    }
  }

/*
 * Get sizes of input arrays.
 */
  mkzh = dsizes_p[ndims_p-3];
  mjx  = dsizes_p[ndims_p-2];
  miy  = dsizes_p[ndims_p-1];

/*
 * Calculate size of leftmost dimensions, and cape array.
 */
  ndims_cape = ndims_p+1;
  dsizes_cape = (int *)calloc(ndims_cape,sizeof(int));
  if(dsizes_cape == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for array dimensionality");
    return(NhlFATAL);
  }

  size_leftmost  = 1;
  dsizes_cape[0] = 2;
  for(i = 0; i < ndims_p-3; i++ ) {
    size_leftmost *= dsizes_p[i];
    dsizes_cape[i+1] = dsizes_p[i];
  }
  dsizes_cape[ndims_p-2] = mkzh;
  dsizes_cape[ndims_p-1] = mjx;
  dsizes_cape[ndims_p]   = miy;

  size_zsfc   = mjx * miy;
  size_cape   = mkzh * size_zsfc;
  size_output = 2 * size_cape * size_leftmost;

/* 
 * Allocate space for output arrays.  If any of the input is already double,
 * then we don't need to allocate space for temporary arrays, because
 * we'll just change the pointer into the void array appropriately.
 */
  if(type_p == NCL_double || type_t == NCL_double || type_q == NCL_double ||
     type_z == NCL_double) {
    type_cape = NCL_double;
    cape = (double *)calloc(size_output,sizeof(double));
    if(cape == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_cape = NCL_float;
    cape      = (float *)calloc(size_output,sizeof(float));
    tmp_cape  = (double *)calloc(size_cape,sizeof(double));
    tmp_cin   = (double *)calloc(size_cape,sizeof(double));
    if(cape == NULL || tmp_cape == NULL || tmp_cin == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for allocating input arrays to double, if necessary.
 */
  if(type_p != NCL_double) {
    tmp_p = (double *)calloc(size_cape,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(size_cape,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_q != NCL_double) {
    tmp_q = (double *)calloc(size_cape,sizeof(double));
    if(tmp_q == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_z != NCL_double) {
    tmp_z = (double *)calloc(size_cape,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_zsfc != NCL_double) {
    tmp_zsfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

  if(type_psfc != NCL_double) {
    tmp_psfc = (double *)calloc(size_zsfc,sizeof(double));
    if(tmp_psfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rip_cape: Unable to allocate memory for coercing input arrays to double");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran routine.
 */ 
  index_cape = index_zsfc = 0;
  index_cin = size_leftmost * size_cape;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subset of input arrays to double if necessary.
 */
    if(type_p != NCL_double) {
      coerce_subset_input_double(p,tmp_p,index_cape,type_p,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_cape];
    }
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_cape,type_t,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_t to appropriate location in t.
 */
      tmp_t = &((double*)t)[index_cape];
    }
    if(type_q != NCL_double) {
      coerce_subset_input_double(q,tmp_q,index_cape,type_q,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_q to appropriate location in q.
 */
      tmp_q = &((double*)q)[index_cape];
    }
    if(type_z != NCL_double) {
      coerce_subset_input_double(z,tmp_z,index_cape,type_z,size_cape,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_cape];
    }

    if(type_psfc != NCL_double) {
      coerce_subset_input_double(psfc,tmp_psfc,index_zsfc,type_psfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_psfc to appropriate location in psfc.
 */
      tmp_psfc = &((double*)psfc)[index_zsfc];
    }
    if(type_zsfc != NCL_double) {
      coerce_subset_input_double(zsfc,tmp_zsfc,index_zsfc,type_zsfc,
                                 size_zsfc,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zsfc to appropriate location in zsfc.
 */
      tmp_zsfc = &((double*)zsfc)[index_zsfc];
    }
    
/*
 * Point tmp_cape and tmp_cin to appropriate location in cape
 * if necessary
 */
    if(type_cape == NCL_double) {
      tmp_cape = &((double*)cape)[index_cape];
      tmp_cin  = &((double*)cape)[index_cin];
    }
    
/*
 * Call Fortran routine.
 */
    NGCALLF(dcapecalc3d,DCAPECALC3D)(tmp_p, tmp_t, tmp_q, tmp_z, tmp_zsfc,
				     tmp_psfc, tmp_cape, tmp_cin, &miy,
				     &mjx, &mkzh, i3dflag, ter_follow);
/*
 * If the output is to be float, then do the coercion here.
 */
    if(type_cape == NCL_float) {
      coerce_output_float_only(cape,tmp_cape,size_cape,index_cape);
      coerce_output_float_only(cape,tmp_cin,size_cape,index_cin);
    }
/*
 * Implement the pointers into the arrays.
 */
    index_cape += size_cape;
    index_cin  += size_cape;
    index_zsfc += size_zsfc;
  }
/*
 * Free memory.
 */
  if(type_p != NCL_double) NclFree(tmp_p);
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_q != NCL_double) NclFree(tmp_q);
  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_cape != NCL_double) NclFree(tmp_cape);
  if(type_cape != NCL_double) NclFree(tmp_cin);
/*
 * Set up variable to return.
 */
  return(NclReturnValue(cape,ndims_cape,dsizes_cape,NULL,type_cape,0));
}

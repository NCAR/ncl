#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(dhydro,DHYDRO)(double *,double *,double *,int *,
                                   double *,int *);

NhlErrorTypes hydro_W( void )
{
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_zh, nlvl, ier=0;
/*
 * Input array variables
 */
  void *p, *tkv, *zsfc;
  double *tmp_p, *tmp_tkv, *tmp_zsfc;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS], has_missing_p;
  int ndims_tkv, dsizes_tkv[NCL_MAX_DIMENSIONS], has_missing_t;
  int ndims_zsfc, dsizes_zsfc[NCL_MAX_DIMENSIONS], has_missing_z;
  NclBasicDataTypes type_p, type_tkv, type_zsfc;
  int size_zsfc, size_p;
/*
 * Output array variables
 */
  void *zh;
  double *tmp_zh;
  NclBasicDataTypes type_zh;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  p = (void*)NclGetArgValue(
          0,
          3,
          &ndims_p,
          dsizes_p,
          NULL,
          &has_missing_p,
          &type_p,
          2);

/*
 * Retrieve argument #2
 */
  tkv = (void*)NclGetArgValue(
          1,
          3,
          &ndims_tkv,
          dsizes_tkv,
          NULL,
          &has_missing_t,
          &type_tkv,
          2);

/*
 * Retrieve argument #3
 */
  zsfc = (void*)NclGetArgValue(
          2,
          3,
          &ndims_zsfc,
          dsizes_zsfc,
          NULL,
          &has_missing_z,
          &type_zsfc,
          2);

/*
 * Check number of dimensions and/or dimension sizes for p and tkv.
 */
  if(ndims_tkv != ndims_p) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p; i++ ) {
    if (dsizes_tkv[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p' and 'tkv' must have the same dimensions");
      return(NhlFATAL);
    }
  }

/*
 * No missing values are allowed.
 */
  if(has_missing_t || has_missing_p || has_missing_z) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The input arrays 'p', 'tkv', and 'zsfc' cannot contain any missing values");
    return(NhlFATAL);
  }

/*
 * Check number of dimensions and/or dimension sizes for zsfc.
 */
  if ((ndims_p == 1 && ndims_zsfc != 1) || 
      (ndims_p > 1 && ndims_zsfc != ndims_p-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The array 'zsfc' must be a scalar or one less dimension than the other arrays");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_p-1; i++ ) {
    if (dsizes_zsfc[i] != dsizes_p[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: The dimensions of the input array 'zsfc' must be the same as the arrays 'p' and 'tkv', minus the last dimension");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the output array.
 */
  nlvl = dsizes_p[ndims_p-1];
  if (nlvl < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: 'nlvl' (the last dimension of 'p' and 'tkv') must be at least one");
    return(NhlFATAL);
  }

  size_zsfc = 1;
  for( i = 0; i < ndims_zsfc; i++ ) size_zsfc *= dsizes_zsfc[i];

  size_p = nlvl * size_zsfc;
/*
 * allocate space for temporary input/output arrays.
 */
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(nlvl,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_tkv != NCL_double) {
    tmp_tkv = (double*)calloc(nlvl,sizeof(double));
    if(tmp_tkv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_zsfc != NCL_double) {
    tmp_zsfc = (double*)calloc(1,sizeof(double));
    if(tmp_zsfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_p != NCL_double && type_tkv != NCL_double &&
     type_zsfc != NCL_double) {

    type_zh = NCL_float;

    zh     = (void*)calloc(size_p,sizeof(float));
    tmp_zh = (double*)calloc(nlvl,sizeof(double));
    if(zh == NULL || tmp_zh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
      return(NhlFATAL);
    }    
  }
  else {
    type_zh = NCL_double;
    zh = (void*)calloc(size_p,sizeof(double));
    if(zh == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"hydro: Unable to allocate memory for output array");
      return(NhlFATAL);
    }    
  }
/*
 * Call the Fortran version of this routine.
 */
  index_zh = 0;
  for( i = 0; i < size_zsfc; i++ ) {
    if(type_p != NCL_double) {
/*
 * Coerce nlvl subsection of p (tmp_p) to double.
 */
      coerce_subset_input_double(p,tmp_p,index_zh,type_p,nlvl,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_p to appropriate location in p.
 */
      tmp_p = &((double*)p)[index_zh];
    }
    if(type_tkv != NCL_double) {
/*
 * Coerce nlvl subsection of tkv (tmp_tkv) to double.
 */
      coerce_subset_input_double(tkv,tmp_tkv,index_zh,type_tkv,nlvl,0,
                                 NULL,NULL);
    }
    else {
/*
 * Point tmp_tkv to appropriate location in tkv.
 */
      tmp_tkv = &((double*)tkv)[index_zh];
    }
    if(type_zsfc != NCL_double) {
/*
 * Coerce subsection of zsfc (tmp_zsfc) to double.
 */
      coerce_subset_input_double(zsfc,tmp_zsfc,i,type_zsfc,1,0,NULL,NULL);
    }
    else {
/*
 * Point tmp_zsfc to appropriate location in zsfc.
 */
      tmp_zsfc = &((double*)zsfc)[i];
    }

    if(type_zh == NCL_double) tmp_zh = &((double*)zh)[index_zh];

    NGCALLF(dhydro,DHYDRO)(tmp_p,tmp_tkv,tmp_zsfc,&nlvl,tmp_zh,&ier);
/*
 * Copy output values from temporary tmp_zh to zh.
 */
    if(type_zh != NCL_double) {
      for(j = 0; j < nlvl; j++) {
        ((float*)zh)[index_zh+j] = (float)(tmp_zh[j]);
      }
    }
    index_zh += nlvl;
  }
/*
 * free memory.
 */
  if(type_p    != NCL_double) NclFree(tmp_p);
  if(type_tkv  != NCL_double) NclFree(tmp_tkv);
  if(type_zsfc != NCL_double) NclFree(tmp_zsfc);
  if(type_zh   != NCL_double) NclFree(tmp_zh);

  return(NclReturnValue(zh,ndims_p,dsizes_p,NULL,type_zh,0));
}

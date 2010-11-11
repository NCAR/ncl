#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dvibeta,DVIBETA)(double *,double *,int *,double *,
                                     int *,double *,double *,double *,
                                     double *,double *,double *,int *);

NhlErrorTypes vibeta_W( void )
{
/*
 * Input array variables
 */
  void *p, *x, *psfc, *pbot, *ptop;
  double *tmp_p = NULL;
  double *tmp_x, *tmp_psfc, *tmp_pbot, *tmp_ptop;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  int has_missing_psfc;
  ng_size_t dsizes_pbot[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_ptop[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_dx, missing_rx;
  NclScalar missing_psfc, missing_dpsfc;
  NclBasicDataTypes type_p, type_x, type_psfc, type_pbot, type_ptop;
  int *linlog;
/*
 * Output array variables
 */
  void *vint;
  double *tmp_vint = NULL;
  NclBasicDataTypes type_vint;
/*
 * various
 */
  int ier;
  ng_size_t i, index_x;
  ng_size_t nlev;
  ng_size_t total_size_x, total_size_psfc;
  int found_missing, inlev;
  double plvcrt, xsfc;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (void*)NclGetArgValue(
                            0,
                            6,
                            &ndims_p, 
                            dsizes_p,
                            NULL,
                            NULL,
                            &type_p,
                            DONT_CARE);

  x = (void*)NclGetArgValue(
                            1,
                            6,
                            &ndims_x, 
                            dsizes_x,
                            &missing_x,
                            &has_missing_x,
                            &type_x,
                            DONT_CARE);
  linlog = (int*)NclGetArgValue(
                                2,
                                6,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                DONT_CARE);
  psfc = (void*)NclGetArgValue(
                               3,
                               6,
                               &ndims_psfc, 
                               dsizes_psfc,
                               &missing_psfc,
                               &has_missing_psfc,
                               &type_psfc,
                               DONT_CARE);
  pbot = (void*)NclGetArgValue(
                               4,
                               6,
                               NULL,
                               dsizes_pbot,
                               NULL,
                               NULL,
                               &type_pbot,
                               DONT_CARE);
  ptop = (void*)NclGetArgValue(
                               5,
                               6,
                               NULL,
                               dsizes_ptop,
                               NULL,
                               NULL,
                               &type_ptop,
                               DONT_CARE);
/*
 * Some error checking.
 */
  if( (ndims_x > 1 && ndims_psfc != ndims_x-1) || 
      (ndims_x == 1 && dsizes_psfc[0] != 1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: 'psfc' must have one less dimension than 'x' or else be a constant" );
    return(NhlFATAL);
  }
/*
 * Calculate size of input arrays.
 */
  total_size_psfc = 1;
  for( i = 0; i < ndims_x-1; i++ ) {
    if( dsizes_psfc[i] != dsizes_x[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: If psfc is not a constant, then its dimension sizes must be equal to all but the last (rightmost) dimension sizes of x");
      return(NhlFATAL);
    }
    total_size_psfc *= dsizes_psfc[i];
  }
  total_size_x = total_size_psfc * dsizes_x[ndims_x-1];

/*
 * p can either be a one-dimensional array with the same size as the
 * last dimension of x, or the same size as x.
 */
  if(ndims_p==1) {
    if( dsizes_p[0] != dsizes_x[ndims_x-1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The rightmost dimension of x and p must be the same size" );
      return(NhlFATAL);
    }
  }
  else {
    if(ndims_p != ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: p must either be one-dimensional and equal in length to the rightmost dimension of x, or p must be the same size as x" );
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_x; i++ ) {
      if( dsizes_p[i] != dsizes_x[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: p must either be one-dimensional and equal in length to the rightmost dimension of x, or p must be the same size as x" );
        return(NhlFATAL);
      }
    }
  }
  nlev = dsizes_p[ndims_p-1];

  if( nlev < 3 || nlev > 150) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: the last dimension of 'p' must be at least 3 and less than 151" );
    return(NhlFATAL);
  }

  if(nlev > INT_MAX){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: nlev = %ld is greater than INT_MAX", nlev);
    return(NhlFATAL);
  }
  inlev = (int) nlev;

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,&missing_dpsfc,NULL);

/*
 * Coerce x to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,total_size_x,has_missing_x,
                              &missing_x,&missing_dx);
  if( tmp_x == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Create memory to allocate p to double precision if necessary.
 * One subset of p at a time will be coerced to double precision when we
 * loop through the leftmost dimensions of psfc.
 */
  if(type_p != NCL_double) {
    tmp_p = (double*)calloc(nlev,sizeof(double));
    if( tmp_p == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing p array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Coerce psfc to double if necessary.
 */
  tmp_psfc = coerce_input_double(psfc,type_psfc,total_size_psfc,
                                 has_missing_psfc,&missing_psfc,
                                 &missing_dpsfc);
  if( tmp_psfc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing psfc array to double precision");
    return(NhlFATAL);
  }
/*
 * Test for presence of missing values in psfc.
 */
  found_missing = contains_missing(tmp_psfc,total_size_psfc,has_missing_psfc,
                                   missing_dpsfc.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: psfc must not contain any missing values" );
    return(NhlFATAL);
  }
/*
 * Coerce pbot to double if necessary.
 */
  tmp_pbot = coerce_input_double(pbot,type_pbot,1,0,NULL,NULL);
  tmp_ptop = coerce_input_double(ptop,type_ptop,1,0,NULL,NULL);
  if(tmp_pbot == NULL || tmp_ptop == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for coercing pbot/ptop arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Some more error checking.
 */
  if(*tmp_ptop >= *tmp_pbot) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ptop must be less than pbot" );
    return(NhlFATAL);
  }

/*
 * Allocate space for output value.
 */
  if(type_x != NCL_double && type_p != NCL_double && 
     type_psfc != NCL_double) {
    type_vint = NCL_float;

    tmp_vint = (double*)calloc(1,sizeof(double));
    vint     = (void*)calloc(total_size_psfc,sizeof(float));
    if(vint == NULL || tmp_vint == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_vint = NCL_double;
    vint = (void*)calloc(total_size_psfc,sizeof(double));
    if(vint == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Call the f77 version of 'vibeta' with the full argument list.
 */
  index_x = 0;
  ier     = 0;
  for( i = 0; i < total_size_psfc; i++ ) {
/*
 * Coerce p to double precision if necessary. If p is one-dimensional,
 * the coercion will only happen the first time through this loop.
 */
    if(ndims_p != 1 || (ndims_p == 1 && !i)) {
      if(type_p != NCL_double) {
        coerce_subset_input_double(p,tmp_p,index_x,type_p,nlev,0,NULL,NULL);
      }
      else {
/*
 * Point tmp_p to p.
 */
        tmp_p = &((double*)p)[index_x];
      }

/*
 * Dennis wanted this test removed. See his email dated November 14, 2003.
 *
 *      if(tmp_p[nlev-1] < *tmp_ptop) {
 *       NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The last element of 'p' must be greater than or equal to ptop" );
 *       return(NhlFATAL);
 *     }
 */
      if(tmp_p[0] <= tmp_p[nlev-1]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: 'p' must be ordered from bottom to top" );
        return(NhlFATAL);
      }
      plvcrt = tmp_p[nlev-1];
    }
/*
 * Set some other input parameters. 
 */
    xsfc = tmp_x[index_x];

    if(type_vint == NCL_double) tmp_vint  = &((double*)vint)[i];

    NGCALLF(dvibeta,DVIBETA)(tmp_p,&tmp_x[index_x],&inlev,&missing_dx.doubleval,
			     linlog,&tmp_psfc[i],&xsfc,tmp_pbot,tmp_ptop,
			     &plvcrt,tmp_vint,&ier);
    if(ier == -999) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"vibeta: there must be at least three levels with data above the surface" );
    }
/*
 * Coerce output to float if necessary.
 */
    if(type_vint != NCL_double) ((float*)vint)[i] = (float)*tmp_vint;
      
    index_x += nlev;
  }

/*
 * free memory.
 */
  if(type_x    != NCL_double) NclFree(tmp_x);
  if(type_p    != NCL_double) NclFree(tmp_p);
  if(type_psfc != NCL_double) NclFree(tmp_psfc);
  if(type_ptop != NCL_double) NclFree(tmp_ptop);
  if(type_pbot != NCL_double) NclFree(tmp_pbot);
  if(type_vint != NCL_double) NclFree(tmp_vint);
/*
 * Check error.
 */
  if(ier) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ier = %d", ier);
    return(NhlFATAL);
  }
/*
 * Return.
 */
  if(type_vint != NCL_double) {
/*
 * None of the input is double, so return float values.
 */
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(vint,ndims_psfc,dsizes_psfc,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(vint,ndims_psfc,dsizes_psfc,&missing_dx,
                          NCL_double,0));
  }
}

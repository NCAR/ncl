#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(spctimcross2,SPCTIMCROSS2)(int *, int *, int *, double *,
                                               double *, double *, int *,
                                               int *, int *, int *, int *,
                                               int *);

extern void NGCALLF(spctimcross3,SPCTIMCROSS3)(int *, int *, double *, 
                                               int *, int *, int *);

NhlErrorTypes mjo_cross_segment_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  ng_size_t dsizes_x[3];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *y;
  double *tmp_y;
  ng_size_t dsizes_y[3];
  int has_missing_y;
  NclScalar missing_y, missing_flt_y, missing_dbl_y;
  NclBasicDataTypes type_y;

/*
 * Argument # 2
 */
  int *opt;
/*
 * Return variable
 */
  void *stc;
  double *tmp_stc;
  ng_size_t dsizes_stc[3];
  NclScalar missing_stc;
  NclBasicDataTypes type_stc;

/*
 * Various
 */
  ng_size_t nt, nm, nl, ntml, nt2p1, nlp1, ntp1, nt2m1, lsave1, lsave2;
  ng_size_t size_stc;
  int i_nt, inm, inl, int2p1, inlp1, int2m1, ilsave1, ilsave2;
  int ret, found_missing_x, found_missing_y;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,
                 &missing_flt_x);

  nt = dsizes_x[0];
  nm = dsizes_x[1];
  nl = dsizes_x[2];
  ntml = nt * nm * nl;

/*
 * Get argument # 1
 */
  y = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(dsizes_y[0] != nt || dsizes_y[1] != nm || dsizes_y[2] != nl) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_segment: The x and y arrays must have the same dimension sizes");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dbl_y,
                 &missing_flt_y);

/*
 * Get argument # 2
 */
  opt = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * The output type defaults to float, unless this input array is double.
 */
  if(type_x == NCL_double || type_y == NCL_double) type_stc = NCL_double;
  else                                             type_stc = NCL_float;

/*
 * Allocate x,y arrays to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,ntml,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,ntml,0,NULL,NULL);

  if(tmp_x == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_segment: Unable to allocate memory for coercing input arrays to double");
    return(NhlFATAL);
  }

/*
 * Test for presence of missing values in tmp_x and tmp_y.
 */
  found_missing_x = contains_missing(tmp_x,ntml,has_missing_x,
                                     missing_dbl_x.doubleval);
  found_missing_y = contains_missing(tmp_y,ntml,has_missing_y,
                                     missing_dbl_y.doubleval);

/* 
 * Size of output array and work arrays that will be 
 * created in Fortran routine.
 */
  nlp1     = nl+1;
  nt2p1    = nt/2+1;
  size_stc = nlp1 * nt2p1 * 16;
  lsave1   = 4*nl + 15;
  lsave2   = 4*nt + 15;
  nt2m1    = nt/2 -1;
  ntp1     = nt + 1;

/*
 * Test dimension sizes.
 */
  if((nl > INT_MAX) || (nm > INT_MAX) || (nt > INT_MAX) ||
     (nlp1 > INT_MAX) || (nt2p1 > INT_MAX) || (nt2m1 > INT_MAX) ||
     (nt2p1 > INT_MAX) || (lsave1 > INT_MAX) || (lsave2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_segment: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inl     = (int) nl;
  inm     = (int) nm;
  i_nt    = (int) nt;
  inlp1   = (int) nlp1;
  int2p1  = (int) nt2p1;
  int2m1  = (int) nt2m1;
  int2p1  = (int) nt2p1;
  ilsave1 = (int) lsave1;
  ilsave2 = (int) lsave2;

/* 
 * Allocate space for output array.
 */
  if(type_stc != NCL_double) {
    stc     = (void *)calloc(size_stc, sizeof(float));
    tmp_stc = (double *)calloc(size_stc,sizeof(double));
    if(tmp_stc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_segment: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    stc     = (void *)calloc(size_stc, sizeof(double));
    tmp_stc = &((double*)stc)[0];

  }
  if(stc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_segment: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  if(found_missing_x || found_missing_y) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"mjo_cross_segment: input array(s) contained missing values. All missing values will be returned");

    if(has_missing_x) {
      if(type_stc == NCL_double) missing_stc = missing_dbl_x;
      else                       missing_stc = missing_flt_x;
      set_subset_output_missing(stc,0,type_stc,size_stc,
                                missing_dbl_x.doubleval);
    }
    else if(has_missing_y) {
      if(type_stc == NCL_double) missing_stc = missing_dbl_y;
      else                       missing_stc = missing_flt_y;
      set_subset_output_missing(stc,0,type_stc,size_stc,
                                missing_dbl_y.doubleval);
    }
/*
 * Set all elements of stc to a missing value.
 */
  }
  else {
/*
 * Call the Fortran routine.
 */
    NGCALLF(spctimcross2,SPCTIMCROSS2)(&inl,&inm,&i_nt,tmp_x,tmp_y,tmp_stc, 
                                       &inlp1,&int2p1,&int2m1,&int2p1,
                                       &ilsave1,&ilsave2);
/*
 * Coerce output back to float if necessary.
 */
    if(type_stc == NCL_float) coerce_output_float_only(stc,tmp_stc,size_stc,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_x   != NCL_double) NclFree(tmp_x);
  if(type_y   != NCL_double) NclFree(tmp_y);
  if(type_stc != NCL_double) NclFree(tmp_stc);

/*
 * Return value back to NCL script.
 */
  
/* 
 * Set dimension sizes for output array.
 */
  dsizes_stc[0] = 16;
  dsizes_stc[1] = nt2p1;
  dsizes_stc[2] = nlp1;

  if(found_missing_x || found_missing_y) {
    ret = NclReturnValue(stc,3,dsizes_stc,&missing_stc,type_stc,0);
  }
  else {
    ret = NclReturnValue(stc,3,dsizes_stc,NULL,type_stc,0);
  }
  return(ret);
}

NhlErrorTypes mjo_cross_coh2pha_W( void )
{
/*
 * Input argument # 0
 */
  void *stc;
  double *tmp_stc;
  ng_size_t dsizes_stc[3], size_stc;
  NclBasicDataTypes type_stc;

/*
 * Input argument # 1
 */
  int *opt;

/*
 * Various
 */
  ng_size_t nl, nt, nlp1, ntp1, nt2p1;
  int inl, i_nt, inlp1, intp1, int2p1;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  stc = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_stc,
           NULL,
           NULL,
           &type_stc,
           DONT_CARE);

  if(type_stc != NCL_float && type_stc != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_coh2pha: The input array must be float or double");
    return(NhlFATAL);
  }
  if(dsizes_stc[0] != 16) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_coh2pha: The leftmost dimension of the input array must be 16");
    return(NhlFATAL);
  }
  nt2p1 = dsizes_stc[1];
  nlp1  = dsizes_stc[2];
  nt    = (nt2p1-1)*2;
  ntp1  = nt + 1;
  nl    = nlp1 - 1;
  size_stc = 16 * nt2p1 * nlp1;

/*
 * Test dimension sizes
 */
  if((nlp1 > INT_MAX) ||
     (ntp1 > INT_MAX) ||
     (nl > INT_MAX) ||
     (nt > INT_MAX) ||
     (nt2p1 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_coh2pha: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inl = (int) nl;
  i_nt = (int) nt;
  inlp1 = (int) nlp1;
  intp1 = (int) ntp1;
  int2p1 = (int) nt2p1;

/*
 * Get argument # 2
 */
  opt = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * Allocate space for coercing input array.  If the input array
 * is already double, then we don't need to allocate space for
 * it, because we'll just point to the void array.
 *
 * Allocate space for tmp_stc.
 */
  tmp_stc = coerce_input_double(stc,type_stc,size_stc,0,NULL,NULL);
  if(tmp_stc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mjo_cross_coh2pha: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routine.
 */
  NGCALLF(spctimcross3,SPCTIMCROSS3)(&inl,&i_nt,tmp_stc,&inlp1,&intp1,&int2p1);

/* 
 * Coerce back to float if necessary.
 */
  if(type_stc == NCL_float) {
    coerce_output_float_only(stc,tmp_stc,size_stc,0);
/*
 * Free unneeded memory.
 */
    NclFree(tmp_stc);
  }
/*
 * This is a procedure, so no values are returned.
 */
  return(NhlNOERROR);
}

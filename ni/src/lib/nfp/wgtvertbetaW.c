#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dwvbetap1,DWVBETAP1)(int *, int *, int *, double *,
                                         double *, double *, double *,
                                         int *, int *, double *, double *,
                                         double *, int *);

extern void NGCALLF(dwvbetap3,DWVBETAP3)(int *, int *, int *, double *,
                                         double *, double *, double *,
                                         int *, int *, double *, double *,
                                         double *, int *);

NhlErrorTypes wgt_vert_avg_beta_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *p;
  double *tmp_p = NULL;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p;

/*
 * Argument # 1
 */
  void *datai;
  double *tmp_datai = NULL;
  int ndims_datai;
  ng_size_t dsizes_datai[NCL_MAX_DIMENSIONS];
  int has_missing_datai;
  NclScalar missing_datai, missing_flt_datai, missing_dbl_datai;
  NclBasicDataTypes type_datai;

/*
 * Argument # 2
 */
  void *psfc;
  double *tmp_psfc = NULL;
  int ndims_psfc;
  ng_size_t dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_psfc;

/*
 * Argument # 3
 */
  int *punits;
/*
 * Argument # 4
 */
  int *opt;
  ng_size_t dsizes_opt[1];
/*
 * Return variable
 */
  void *wva;
  double *tmp_wva = NULL;
  int ndims_wva;
  ng_size_t *dsizes_wva;
  NclBasicDataTypes type_wva;

/*
 * Various
 */
  double ptop = 0, pbot=1100;
  int ier;
  ng_size_t klev, nlat, mlon, klevnlatmlon, nlatmlon, nopt;
  ng_size_t index_datai, index_psfc;
  ng_size_t i, size_leftmost, size_output;
  int ndims_leftmost;
  NhlErrorTypes ret;
  int imlon, inlat, iklev;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  p = (void*)NclGetArgValue(
           0,
           5,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           DONT_CARE);

/*
 * Get size of level dimension.
 */
  if(ndims_p > 1) {
    klev = dsizes_p[ndims_p-3];
  }
  else {
    klev = dsizes_p[0];
  }

/*
 * Get argument # 1
 */
  datai = (void*)NclGetArgValue(
           1,
           5,
           &ndims_datai,
           dsizes_datai,
           &missing_datai,
           &has_missing_datai,
           &type_datai,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_datai < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The datai array must have at least 3 dimensions");
    return(NhlFATAL);
  }

/*
 * Check dimension sizes of p and datai against each other, if p is nD.
 */
  if(ndims_p < 1 || ndims_p == 2 || (ndims_p > 1 && ndims_p != ndims_datai)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The p array must either be a one dimensional array of length klev, or at least a 3-dimensional array of the same size as datai, with rightmost dimensions klev x nlat x mlon");
    return(NhlFATAL);
  }

  if(dsizes_datai[ndims_datai-3] != klev) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The third-to-the-last dimension of datai must be of length klev");
    return(NhlFATAL);
  }
  nlat         = dsizes_datai[ndims_datai-2];
  mlon         = dsizes_datai[ndims_datai-1];
  nlatmlon     = nlat * mlon;
  klevnlatmlon = klev * nlatmlon;

  if((ndims_p > 1) && (dsizes_p[ndims_p-2] != nlat || dsizes_p[ndims_p-1] != mlon)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The p array must either be a one dimensional array of klev values, or an array whose rightmost three dimensions are klev x nlat x mlon");
    return(NhlFATAL);
  }

/* 
 * Test dimension sizes.
 */
  if((mlon > INT_MAX) || (nlat > INT_MAX) || (klev > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: one or more dimension sizes is greater than INT_MAX", mlon);
    return(NhlFATAL);
  }
  imlon = (int) mlon;
  inlat = (int) nlat;
  iklev = (int) klev;

/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_datai,has_missing_datai,&missing_datai,
                 &missing_dbl_datai,&missing_flt_datai);

/*
 * Get argument # 2
 */
  psfc = (void*)NclGetArgValue(
           2,
           5,
           &ndims_psfc,
           dsizes_psfc,
           NULL,
           NULL,
           &type_psfc,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_psfc < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The psfc array must have at least 2 dimensions");
    return(NhlFATAL);
  }
  if(ndims_psfc != (ndims_datai-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The psfc array must have one less dimension than datai");
    return(NhlFATAL);
  }
  if(dsizes_psfc[ndims_psfc-2] != nlat || dsizes_psfc[ndims_psfc-1] != mlon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The rightmost two dimensions of psfc must be nlat x mlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  punits = (int*)NclGetArgValue(
           3,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 4
 *
 * opt is both a toggle, and, if it is length 3, it also determines the
 * values of ptop and pbot.
 */
  opt = (int*)NclGetArgValue(
           4,
           5,
           NULL,
           dsizes_opt,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  nopt = dsizes_opt[0];
  if(nopt != 1 && nopt != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: opt must either be a scalar or an array of length 3");
    return(NhlFATAL);
  }

  if(nopt == 3) {
    ptop = (double)opt[1];
    pbot = (double)opt[2];
  }
/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = ndims_datai-3;
  for(i = 0; i < ndims_leftmost; i++) {
    if(ndims_p > 1) {
      if(dsizes_p[i] != dsizes_datai[i] || dsizes_psfc[i] != dsizes_datai[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The leftmost dimensions of p, datai and psfc must be the same");
        return(NhlFATAL);
      }
    }
    else {
      if(dsizes_psfc[i] != dsizes_datai[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: The leftmost dimensions of datai and psfc must be the same");
        return(NhlFATAL);
      }
    }
    size_leftmost *= dsizes_datai[i];
  }

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 *
 * The output type defaults to float, unless psfc or datai are double.
 */
  type_wva = NCL_float;
/*
 * Allocate space for tmp_p.  If p is 1D, then we should coerce it
 * now. Otherwise, we will create a temporary array to hold
 * klev x nlat x mlon values, and coerce the values later inside
 * the loop across the leftmost dimensions.
 */
  if(ndims_p == 1) {
    tmp_p = coerce_input_double(p,type_p,klev,0,NULL,NULL);
  }
  else if(type_p != NCL_double) {
    tmp_p = (double *)calloc(klevnlatmlon,sizeof(double));
    if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for tmp_datai.
 */
  if(type_datai != NCL_double) {
    tmp_datai = (double *)calloc(klevnlatmlon,sizeof(double));
    if(tmp_datai == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_wva = NCL_double;
  }
/*
 * Allocate space for tmp_psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double *)calloc(nlatmlon,sizeof(double));
    if(tmp_psfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_wva = NCL_double;
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftmost * nlatmlon;

/* 
 * Allocate space for output array.
 */
  if(type_wva != NCL_double) {
    wva     = (void *)calloc(size_output, sizeof(float));
    tmp_wva = (double *)calloc(nlatmlon,sizeof(double));
    if(tmp_wva == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    wva = (void *)calloc(size_output, sizeof(double));
  }
  if(wva == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_wva = ndims_leftmost + 2;
  dsizes_wva = (ng_size_t*)calloc(ndims_wva,sizeof(ng_size_t));  
  if( dsizes_wva == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_vert_avg_beta: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_wva-2; i++) dsizes_wva[i] = dsizes_datai[i];
  dsizes_wva[ndims_wva-2] = nlat;
  dsizes_wva[ndims_wva-1] = mlon;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * one-dimensional subsection.
 */
  index_datai = index_psfc = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of p (tmp_p) to double if necessary.  If p is 1D,
 * then the coercion should have already taken place.
 */
    if(ndims_p > 1) {
      if(type_p != NCL_double) {
        coerce_subset_input_double(p,tmp_p,index_datai,type_p,klevnlatmlon,0,
                                   NULL,NULL);
      }
      else {
        tmp_p = &((double*)p)[index_datai];
      }
    }

/*
 * Coerce subsection of datai (tmp_datai) to double if necessary.
 */
    if(type_datai != NCL_double) {
      coerce_subset_input_double(datai,tmp_datai,index_datai,type_datai,
                                 klevnlatmlon,0,NULL,NULL);
    }
    else {
      tmp_datai = &((double*)datai)[index_datai];
    }

/*
 * Coerce subsection of psfc (tmp_psfc) to double if necessary.
 */
    if(type_psfc != NCL_double) {
      coerce_subset_input_double(psfc,tmp_psfc,index_psfc,type_psfc,
                                 nlatmlon,0,NULL,NULL);
    }
    else {
      tmp_psfc = &((double*)psfc)[index_psfc];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_wva == NCL_double) tmp_wva = &((double*)wva)[index_psfc];

/*
 * Call one of the two Fortran routines, depending on whether p is
 * 1D or the same dimensions as datai.
 */
    if(ndims_p == 1) {
      NGCALLF(dwvbetap1,DWVBETAP1)(&imlon, &inlat, &iklev, tmp_p, tmp_datai,
				   &missing_dbl_datai.doubleval, tmp_psfc,
				   punits, &opt[0], &ptop, &pbot, tmp_wva,
				   &ier);
    }
    else {
      NGCALLF(dwvbetap3,DWVBETAP3)(&imlon, &inlat, &iklev, tmp_p, tmp_datai,
				   &missing_dbl_datai.doubleval, tmp_psfc,
				   punits, &opt[0], &ptop, &pbot, tmp_wva,
				   &ier);
    }

/*
 * Coerce output back to float if necessary.
 */
    if(type_wva == NCL_float) {
      coerce_output_float_only(wva,tmp_wva,nlatmlon,index_psfc);
    }
    index_datai += klevnlatmlon;
    index_psfc  += nlatmlon;
  }

/*
 * Free unneeded memory.
 */
  if(type_p     != NCL_double) NclFree(tmp_p);
  if(type_datai != NCL_double) NclFree(tmp_datai);
  if(type_psfc  != NCL_double) NclFree(tmp_psfc);
  if(type_wva   != NCL_double) NclFree(tmp_wva);

/*
 * Return value back to NCL script.
 */
  if(has_missing_datai) {
    if(type_wva != NCL_double) {
     ret = NclReturnValue(wva,ndims_wva,dsizes_wva,&missing_flt_datai,
                            type_wva,0);
    }
    else {
      ret = NclReturnValue(wva,ndims_wva,dsizes_wva,&missing_dbl_datai,
                            type_wva,0);
    }
  }
  else {
    ret =NclReturnValue(wva,ndims_wva,dsizes_wva,NULL,type_wva,0);
  }
  NclFree(dsizes_wva);
  return ret;
}

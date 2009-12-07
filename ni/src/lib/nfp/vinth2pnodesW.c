#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dvinth2pnodes,DVINTH2PNODES)(double *dati, double *dato, 
                                     double *hbcofa, double *hbcofb, 
                                     double *p0, double *plevi, 
                                     double *plevo, int *intyp, int *ilev, 
                                     double *psfc, double *spvl, int *kxtrp,
                                     int *npts, int *nlevi, int *nlevip1, 
                                     int *nlevo);


NhlErrorTypes vinth2p_nodes_W( void )
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *datai;
  double *tmp_datai;
  int ndims_datai, dsizes_datai[NCL_MAX_DIMENSIONS];
  int has_missing_datai;
  NclScalar missing_datai, missing_flt_datai, missing_dbl_datai;
  NclBasicDataTypes type_datai;

/*
 * Argument # 1
 */
  void *hbcofa;
  double *tmp_hbcofa;
  int dsizes_hbcofa[1];
  NclBasicDataTypes type_hbcofa;

/*
 * Argument # 2
 */
  void *hbcofb;
  double *tmp_hbcofb;
  int dsizes_hbcofb[1];
  NclBasicDataTypes type_hbcofb;

/*
 * Argument # 3
 */
  void *plevo;
  double *tmp_plevo;
  int dsizes_plevo[1];
  NclBasicDataTypes type_plevo;

/*
 * Argument # 4
 */
  void *psfc;
  double *tmp_psfc;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  int has_missing_psfc;
  NclScalar missing_psfc, missing_flt_psfc, missing_dbl_psfc;
  NclBasicDataTypes type_psfc;

/*
 * Argument # 5
 */
  int *intyp;
/*
 * Argument # 6
 */
  void *p0;
  double *tmp_p0;
  NclBasicDataTypes type_p0;

/*
 * Argument # 7
 */
  int *ilev;
/*
 * Argument # 8
 */
  logical *kxtrp;
/*
 * Return variable
 */
  void *datao;
  double *tmp_datao;
  int ndims_datao, *dsizes_datao;
  int has_missing_datao;
  NclScalar missing_datao, missing_flt_datao, missing_dbl_datao;
  NclBasicDataTypes type_datao;


/*
 * Various
 */
  int nlevi, npts, nlevinpts, nlevip1, nlevo, nlevonpts;
  int index_datai, index_psfc, index_datao;
  double *plevi;
  int i, ndims_leftmost, size_leftmost, size_output, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  datai = (void*)NclGetArgValue(
           0,
           9,
           &ndims_datai,
           dsizes_datai,
           &missing_datai,
           &has_missing_datai,
           &type_datai,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_datai < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: The datai array must have at least 2 dimensions");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_datai,has_missing_datai,&missing_datai,
                 &missing_dbl_datai,&missing_flt_datai);

  nlevi = dsizes_datai[ndims_datai-2];
  npts  = dsizes_datai[ndims_datai-1];
  nlevinpts = nlevi * npts;

/*
 * Get argument # 1
 */
  hbcofa = (void*)NclGetArgValue(
          1,
          9,
          NULL,
          dsizes_hbcofa,
          NULL,
          NULL,
          &type_hbcofa,
          DONT_CARE);

  nlevip1 = dsizes_hbcofa[0];
  if(nlevip1 != (nlevi+1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: The length of hbcofa/hbcofb must be nlevi+1");
    return(NhlFATAL);
  }
    

/*
 * Get argument # 2
 */
  hbcofb = (void*)NclGetArgValue(
           2,
           9,
           NULL,
           dsizes_hbcofb,
           NULL,
           NULL,
           &type_hbcofb,
           DONT_CARE);

  if(dsizes_hbcofb[0] != nlevip1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: The length of hbcofb must be the same as hbcofa");
    return(NhlFATAL);
  }
/*
 * Get argument # 3
 */
  plevo = (void*)NclGetArgValue(
           3,
           9,
           NULL,
           dsizes_plevo,
           NULL,
           NULL,
           &type_plevo,
           DONT_CARE);

  nlevo = dsizes_plevo[0];

/*
 * Get argument # 4
 */
  psfc = (void*)NclGetArgValue(
           4,
           9,
           &ndims_psfc,
           dsizes_psfc,
           &missing_psfc,
           &has_missing_psfc,
           &type_psfc,
           DONT_CARE);

  if(ndims_psfc != (ndims_datai-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: psfc must have one fewer dimensions than datai");
    return(NhlFATAL);
  }

  if(dsizes_psfc[ndims_psfc-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: The rightmost dimension of psfc must be of length npts");
    return(NhlFATAL);
  }
/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_psfc,has_missing_psfc,&missing_psfc,
                 &missing_dbl_psfc,&missing_flt_psfc);

/*
 * Get argument # 5
 */
  intyp = (int*)NclGetArgValue(
           5,
           9,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 6
 */
  p0 = (void*)NclGetArgValue(
           6,
           9,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_p0,
           DONT_CARE);
/*
 * Get argument # 7
 */
  ilev = (int*)NclGetArgValue(
           7,
           9,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * Get argument # 8
 */
  kxtrp = (logical*)NclGetArgValue(
           8,
           9,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = ndims_datai-2;
  for(i = 0; i < ndims_leftmost; i++) {
    if(dsizes_psfc[i] != dsizes_datai[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: The leftmost dimensions of datai and psfc must be the same");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_datai[i];
  }

/*
 * The output type defaults to float, unless one of two input array
 * are double.
 */
  if(type_datai == NCL_double || type_psfc == NCL_double) {
    type_datao = NCL_double;
  }
  else {
    type_datao = NCL_float;
  }

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_datai.
 */
  if(type_datai != NCL_double) {
    tmp_datai = (double *)calloc(nlevinpts,sizeof(double));
    if(tmp_datai == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for tmp_hbcofa.
 */
  tmp_hbcofa = coerce_input_double(hbcofa,type_hbcofa,nlevip1,0,NULL,NULL);
  if(tmp_hbcofa == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_hbcofb.
 */
  tmp_hbcofb = coerce_input_double(hbcofb,type_hbcofb,nlevip1,0,NULL,NULL);
  if(tmp_hbcofb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_plevo.
 */
  tmp_plevo = coerce_input_double(plevo,type_plevo,nlevo,0,NULL,NULL);
  if(tmp_plevo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_psfc.
 */
  if(type_psfc != NCL_double) {
    tmp_psfc = (double *)calloc(npts,sizeof(double));
    if(tmp_psfc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for tmp_p0.
 */
  tmp_p0 = coerce_input_double(p0,type_p0,1,0,NULL,NULL);
  if(tmp_p0 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for plevi.
 */
  plevi = (double *)calloc(nlevip1,sizeof(double));
  if(plevi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for plevi array");
    return(NhlFATAL);
  }
  
/*
 * Calculate size of output array.
 */
  nlevonpts = nlevo * npts;
  size_output = size_leftmost * nlevonpts;

/* 
 * Allocate space for output array.
 */
  if(type_datao != NCL_double) {
    datao     = (void *)calloc(size_output, sizeof(float));
    tmp_datao = (double *)calloc(nlevonpts,sizeof(double));
    if(tmp_datao == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    datao = (void *)calloc(size_output, sizeof(double));
  }
  if(datao == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_datai) {
    if(type_datao == NCL_double) missing_datao = missing_dbl_datai;
    else                         missing_datao = missing_flt_datai;
    missing_dbl_datao =          missing_dbl_datai;
  }
  else if(has_missing_psfc) {
    if(type_datao == NCL_double) missing_datao = missing_dbl_psfc;
    else                         missing_datao = missing_flt_psfc;
    missing_dbl_datao =          missing_dbl_psfc;
  }
  else {
/*
 * Don't use NCL default of -999 or -9999.
 */
    if(type_datao == NCL_double) missing_datao.doubleval = 1.e20;
    else                         missing_datao.floatval  = 1.e20;
    missing_dbl_datao.doubleval = 1.e20;
  }
/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_datao = ndims_leftmost + 2;
  dsizes_datao = (int*)calloc(ndims_datao,sizeof(int));  
  if( dsizes_datao == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vinth2p_nodes: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_datao-2; i++) dsizes_datao[i] = dsizes_datai[i];
  dsizes_datao[ndims_datao-2] = nlevo;
  dsizes_datao[ndims_datao-1] = npts;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_datai = index_psfc = index_datao = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of datai (tmp_datai) to double if necessary.
 */
    if(type_datai != NCL_double) {
      coerce_subset_input_double(datai,tmp_datai,index_datai,type_datai,
                                 nlevinpts,0,NULL,NULL);
    }
    else {
      tmp_datai = &((double*)datai)[index_datai];
    }

/*
 * Coerce subsection of psfc (tmp_psfc) to double if necessary.
 */
    if(type_psfc != NCL_double) {
            coerce_subset_input_double(psfc,tmp_psfc,index_psfc,type_psfc,npts,
                                       0,NULL,NULL);
    }
    else {
      tmp_psfc = &((double*)psfc)[index_psfc];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_datao == NCL_double) tmp_datao = &((double*)datao)[index_datao];


/*
 * Call the Fortran routine.
 */
    printf("msg = %g\n", missing_dbl_datai.doubleval);
    NGCALLF(dvinth2pnodes,DVINTH2PNODES)(tmp_datai, tmp_datao, tmp_hbcofa, 
                                         tmp_hbcofb, tmp_p0, plevi, tmp_plevo,
                                         intyp, ilev, tmp_psfc, 
                                         &missing_dbl_datao.doubleval, kxtrp,
                                         &npts, &nlevi, &nlevip1, &nlevo);

/*
 * Coerce output back to float if necessary.
 */
    if(type_datao == NCL_float) {
      coerce_output_float_only(datao,tmp_datao,nlevonpts,index_datao);
    }
    index_datai += nlevinpts;
    index_psfc  += npts;
    index_datao += nlevonpts;
  }

/*
 * Free unneeded memory.
 */
  if(type_datai  != NCL_double) NclFree(tmp_datai);
  if(type_hbcofa != NCL_double) NclFree(tmp_hbcofa);
  if(type_hbcofb != NCL_double) NclFree(tmp_hbcofb);
  if(type_plevo  != NCL_double) NclFree(tmp_plevo);
  if(type_psfc   != NCL_double) NclFree(tmp_psfc);
  if(type_p0     != NCL_double) NclFree(tmp_p0);
  if(type_datao  != NCL_double) NclFree(tmp_datao);
  NclFree(plevi);

/*
 * Return value back to NCL script.
 */
  if(type_datao == NCL_double) {
     ret = NclReturnValue(datao,ndims_datao,dsizes_datao,
                          &missing_dbl_datao,type_datao,0);
  }
  else {
     ret = NclReturnValue(datao,ndims_datao,dsizes_datao,
                          &missing_flt_datao,type_datao,0);
  }
  NclFree(dsizes_datao);
  return(ret);
}

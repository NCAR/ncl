#include <stdio.h>
#include <string.h>
#include <math.h>
#include "wrapper.h"


extern void NGCALLF(dsvdlap,DSVDLAP)(double *,double *,int *,int *,int *,
                                     int *,int *,int *,double  *,int *,
                                     double *,int *,double *, double *,
                                     double *, double *, double *,double *,
                                     double *, int *, int *);

extern void NGCALLF(dsvdsv,DSVDSV)(double *,double *, int *,int *,int *,
                                   int *,int *,int *,double *, int *,
                                   double *, double *,double *, double *,
                                   double *,double *, double *,int *,
                                   double *,int *,int *);

extern void NGCALLF(dsvdpar,DSVDPAR)(double *,int *,int*,int*,char*,int);

extern void NGCALLF(dgesvd,DGESVD)(char *, char *, int *, int *, double *,
                                   int *, double *, double *, int *, 
                                   double *, int*, double *, int *, int *);


NhlErrorTypes svdcov_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *homlft, *hetlft, *homrgt, *hetrgt;
  double *dx, *dy, *homlft_tmp, *hetlft_tmp, *homrgt_tmp, *hetrgt_tmp;
  ng_size_t dsizes_x[2];
  int has_missing_x;
  ng_size_t dsizes_y[2];
  int has_missing_y;
  ng_size_t dsizes_homlft[2];
  ng_size_t dsizes_hetlft[2];
  ng_size_t dsizes_homrgt[2];
  ng_size_t dsizes_hetrgt[2];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  NclBasicDataTypes type_homlft, type_hetlft, type_homrgt, type_hetrgt;
  ng_size_t ntimes, ncolx, ncoly, lab;
  int *nsvd;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w;
  ng_size_t lwk, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv_tmp, *ak_tmp, *bk_tmp;
  void *svdpcv, *fnorm, *condn, *ak, *bk;
  int *lapack_err;
  ng_size_t dsizes_svdpcv[1];
  NclBasicDataTypes type_svdpcv;
  NclTypeClass type_svdpcv_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  ng_size_t ipt7;
  ng_size_t total_size_x, total_size_y, total_size_lft, total_size_rgt;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           7,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           7,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
  homlft = (void*)NclGetArgValue(
           3,
           7,
           NULL,
           dsizes_homlft,
           NULL,
           NULL,
           &type_homlft,
           DONT_CARE);

  hetlft = (void*)NclGetArgValue(
           4,
           7,
           NULL,
           dsizes_hetlft,
           NULL,
           NULL,
           &type_hetlft,
           DONT_CARE);


  homrgt = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           dsizes_homrgt,
           NULL,
           NULL,
           &type_homrgt,
           DONT_CARE);

  hetrgt = (void*)NclGetArgValue(
           6,
           7,
           NULL,
           dsizes_hetrgt,
           NULL,
           NULL,
           &type_hetrgt,
           DONT_CARE);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The rightmost dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];
  lab    = ntimes * *nsvd;
  nsvmx  = min(ncolx,ncoly);

/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > nsvmx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.  The first dimension must be nsvd. The second
 * dimension must be the same as the first dimension of x (or y if
 * it's the rgt arrays).
 */
  if( dsizes_homlft[0] != *nsvd || dsizes_hetlft[0] != *nsvd ||
      dsizes_homlft[1] != ncolx || dsizes_hetlft[1] != ncolx ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The rightmost dimension of the homlft/hetlft arrays must be the same as the leftmost dimension of x, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_homrgt[0] != *nsvd || dsizes_hetrgt[0] != *nsvd ||
      dsizes_homrgt[1] != ncoly || dsizes_hetrgt[1] != ncoly ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The rightmost dimension of the homrgt/hetrgt arrays must be the same as the leftmost dimension of y, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * The hom/het arrays must be float or double. It doesn't matter what
 * the input types are.
 */
  if((type_hetlft != NCL_float && type_hetlft != NCL_double) ||
     (type_homlft != NCL_float && type_homlft != NCL_double) ||
     (type_homrgt != NCL_float && type_homrgt != NCL_double) ||
     (type_hetrgt != NCL_float && type_hetrgt != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The hetlft/homlft/hetrgt/homrgt arrays must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Calculate total sizes of input arrays.
 */
  total_size_x = ncolx * ntimes;
  total_size_y = ncoly * ntimes;

  total_size_lft = *nsvd * ncolx;
  total_size_rgt = *nsvd * ncoly;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x and y to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_y,has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision hom/het. There's no need to do a
 * coercion because hom/het is an output-only variable (i.e, there are no
 * values coming in).  hom/het can only be float or double, so only
 * allocate space for a d.p. array if hom/het is float.
 */
  homlft_tmp = coerce_output_double(homlft,type_homlft,total_size_lft);
  hetlft_tmp = coerce_output_double(hetlft,type_hetlft,total_size_lft);
  homrgt_tmp = coerce_output_double(homrgt,type_homrgt,total_size_rgt);
  hetrgt_tmp = coerce_output_double(hetrgt,type_hetrgt,total_size_rgt);
  if(homlft_tmp == NULL || hetlft_tmp == NULL || 
     homrgt_tmp == NULL || hetrgt_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Determine type of output values and allocate space for them.
 */
  dsizes_svdpcv[0] = *nsvd;
  if(type_x != NCL_double && type_y != NCL_double) {
    type_svdpcv = NCL_float;
    svdpcv      = (void *)calloc(*nsvd,sizeof(float));
    ak          = (void *)calloc(lab,sizeof(float));
    bk          = (void *)calloc(lab,sizeof(float));
    svdpcv_tmp  = (double *)calloc(*nsvd,sizeof(double));
    ak_tmp      = (double *)calloc(lab,sizeof(double));
    bk_tmp      = (double *)calloc(lab,sizeof(double));

    if( svdpcv == NULL || svdpcv_tmp == NULL || ak == NULL || bk == NULL ||
        ak_tmp == NULL || bk_tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for output values");
      return(NhlFATAL);
    }
  }
  else {
    type_svdpcv = NCL_double;
    svdpcv      = (void *)calloc(*nsvd,sizeof(double));
    ak          = (void *)calloc(lab,sizeof(double));
    bk          = (void *)calloc(lab,sizeof(double));
    if( svdpcv == NULL || ak == NULL || bk == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for output values");
      return(NhlFATAL);
    }
    svdpcv_tmp  = (double *)svdpcv;
    ak_tmp      = (double *)ak;
    bk_tmp      = (double *)bk;
  }

/*
 * Allocate memory for work array. Note that this work array is actually
 * serving as a work array, plus memory for 6 other arrays in the 
 * Fortran routine (CRV, U, VT, SV, AK, BK).
 *
 * Originally we had "5*min(ncolx,ncoly)-4", but the SGI version of 
 * the DGESVD routine recommends "5*min(ncolx,ncoly)", so we decided
 * to take the bigger of the two, and go with "5*min(ncolx,ncoly)".
 */
  lwk = ncolx*ncoly + nsvmx*ncolx + nsvmx*ncoly +
        ntimes*ncolx + ntimes*ncoly + nsvmx +
        max(3*nsvmx + max(ncolx,ncoly), 5*nsvmx);
  w = (double *)calloc(lwk,sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  if((ntimes <= INT_MAX) &&
     (ncolx <= INT_MAX) &&
     (ncoly <= INT_MAX) &&
     (lab <= INT_MAX) &&
     (lwk <= INT_MAX))
  {
    int intimes = (int) ntimes;
    int incolx = (int) ncolx;
    int incoly = (int) ncoly;
    int ilab = (int) lab;
    int ilwk = (int) lwk;
    NGCALLF(dsvdlap,DSVDLAP)(dx,dy,&intimes,&intimes,&incolx,&incoly,nsvd,&iflag,
                             &missing_dx.doubleval,&iprint,w,&ilwk,svdpcv_tmp,
                             homlft_tmp,homrgt_tmp,hetlft_tmp,hetrgt_tmp,
                             ak_tmp,bk_tmp,&ilab,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }

  if (ier) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"svdcov: ier = %d\n", ier );
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

/*
 * If hom/het were originally float, then we need to coerce them from
 * double to float.
 */
  if(type_homlft == NCL_float) {
    coerce_output_float_only(homlft,homlft_tmp,total_size_lft,0);
    NclFree(homlft_tmp);
  }

  if(type_hetlft == NCL_float) {
    coerce_output_float_only(hetlft,hetlft_tmp,total_size_lft,0);
    NclFree(hetlft_tmp);
  }

  if(type_homrgt == NCL_float) {
    coerce_output_float_only(homrgt,homrgt_tmp,total_size_rgt,0);
    NclFree(homrgt_tmp);
  }

  if(type_hetrgt == NCL_float) {
    coerce_output_float_only(hetrgt,hetrgt_tmp,total_size_rgt,0);
    NclFree(hetrgt_tmp);
  }

/*
 *  Assign values for various attributes.
 */
  ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;

  if(type_svdpcv == NCL_float) {
    fnorm  = (void *)calloc(1,sizeof(float));
    condn  = (void *)calloc(1,sizeof(float));
    coerce_output_float_only(svdpcv,svdpcv_tmp,*nsvd,0);
    coerce_output_float_only(ak,ak_tmp,lab,0);
    coerce_output_float_only(bk,bk_tmp,lab,0);
    NclFree(svdpcv_tmp);
    NclFree(ak_tmp);
    NclFree(bk_tmp);
  }
  else {
    fnorm  = (void *)calloc(1,sizeof(double));
    condn  = (void *)calloc(1,sizeof(double));
  }
  coerce_output_float_or_double(fnorm,&w[ipt7],type_svdpcv,1,0);
  coerce_output_float_or_double(condn,&w[ipt7+1],type_svdpcv,1,0);
  lapack_err  = (int *)calloc(1,sizeof(int));
  *lapack_err = (int)w[ipt7+3];

/*
 * Set up variable to return.
 */
  type_svdpcv_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_svdpcv)));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            svdpcv,
                            NULL,
                            1,
                            dsizes_svdpcv,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_svdpcv_class
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Create individual attributes.
 */
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         fnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         condn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
             att_md,
             NULL
             );

  dsizes[0] = lab;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         ak,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "ak",
             att_md,
             NULL
             );


  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         bk,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "bk",
             att_md,
             NULL
             );


  NclFree(w);

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


NhlErrorTypes svdstd_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *homlft, *hetlft, *homrgt, *hetrgt;
  double *dx, *dy, *homlft_tmp, *hetlft_tmp, *homrgt_tmp, *hetrgt_tmp;
  ng_size_t dsizes_x[2];
  int has_missing_x;
  ng_size_t dsizes_y[2];
  int has_missing_y;
  ng_size_t dsizes_homlft[2];
  ng_size_t dsizes_hetlft[2];
  ng_size_t dsizes_homrgt[2];
  ng_size_t dsizes_hetrgt[2];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  NclBasicDataTypes type_homlft, type_hetlft, type_homrgt, type_hetrgt;
  ng_size_t ntimes, ncolx, ncoly, lab;
  int *nsvd;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w;
  ng_size_t lwk, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv_tmp, *ak_tmp, *bk_tmp;
  void *svdpcv, *fnorm, *condn, *ak, *bk;
  int *lapack_err;
  ng_size_t dsizes_svdpcv[1];
  NclBasicDataTypes type_svdpcv;
  NclTypeClass type_svdpcv_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  ng_size_t ipt7;
  ng_size_t total_size_x, total_size_y, total_size_lft, total_size_rgt;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           7,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           7,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);

  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
  homlft = (void*)NclGetArgValue(
           3,
           7,
           NULL,
           dsizes_homlft,
           NULL,
           NULL,
           &type_homlft,
           DONT_CARE);

  hetlft = (void*)NclGetArgValue(
           4,
           7,
           NULL,
           dsizes_hetlft,
           NULL,
           NULL,
           &type_hetlft,
           DONT_CARE);


  homrgt = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           dsizes_homrgt,
           NULL,
           NULL,
           &type_homrgt,
           DONT_CARE);

  hetrgt = (void*)NclGetArgValue(
           6,
           7,
           NULL,
           dsizes_hetrgt,
           NULL,
           NULL,
           &type_hetrgt,
           DONT_CARE);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The rightmost dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];
  lab    = ntimes * *nsvd;
  nsvmx  = min(ncolx,ncoly);

/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > nsvmx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.  The first dimension must be nsvd. The second
 * dimension must be the same as the first dimension of x (or y if
 * it's the rgt arrays).
 */
  if( dsizes_homlft[0] != *nsvd || dsizes_hetlft[0] != *nsvd ||
      dsizes_homlft[1] != ncolx || dsizes_hetlft[1] != ncolx ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The rightmost dimension of the homlft/hetlft arrays must be the same as the leftmost dimension of x, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_homrgt[0] != *nsvd || dsizes_hetrgt[0] != *nsvd ||
      dsizes_homrgt[1] != ncoly || dsizes_hetrgt[1] != ncoly ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The rightmost dimension of the homrgt/hetrgt arrays must be the same as the leftmost dimension of y, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * The hom/het arrays must be float or double. It doesn't matter what
 * the input types are.
 */
  if((type_hetlft != NCL_float && type_hetlft != NCL_double) ||
     (type_homlft != NCL_float && type_homlft != NCL_double) ||
     (type_homrgt != NCL_float && type_homrgt != NCL_double) ||
     (type_hetrgt != NCL_float && type_hetrgt != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The hetlft/homlft/hetrgt/homrgt arrays must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Calculate total sizes of input arrays.
 */
  total_size_x = ncolx * ntimes;
  total_size_y = ncoly * ntimes;

  total_size_lft = *nsvd * ncolx;
  total_size_rgt = *nsvd * ncoly;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x and y to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_y,has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision hom/het. There's no need to do a
 * coercion because hom/het is an output-only variable (i.e, there are no
 * values coming in).  hom/het can only be float or double, so only
 * allocate space for a d.p. array if hom/het is float.
 */
  homlft_tmp = coerce_output_double(homlft,type_homlft,total_size_lft);
  hetlft_tmp = coerce_output_double(hetlft,type_hetlft,total_size_lft);
  homrgt_tmp = coerce_output_double(homrgt,type_homrgt,total_size_rgt);
  hetrgt_tmp = coerce_output_double(hetrgt,type_hetrgt,total_size_rgt);
  if(homlft_tmp == NULL || hetlft_tmp == NULL || 
     homrgt_tmp == NULL || hetrgt_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Determine type of output values and allocate space for them.
 */
  dsizes_svdpcv[0] = *nsvd;
  if(type_x != NCL_double && type_y != NCL_double) {
    type_svdpcv = NCL_float;
    svdpcv     = (void *)calloc(*nsvd,sizeof(float));
    svdpcv_tmp = (double *)calloc(*nsvd,sizeof(double));
    ak_tmp     = (double *)calloc(lab,sizeof(double));
    bk_tmp     = (double *)calloc(lab,sizeof(double));
    ak         = (void *)calloc(lab,sizeof(float));
    bk         = (void *)calloc(lab,sizeof(float));

    if( svdpcv == NULL || svdpcv_tmp == NULL || ak == NULL || bk == NULL ||
        ak_tmp == NULL || bk_tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for output values");
      return(NhlFATAL);
    }
  }
  else {
    type_svdpcv = NCL_double;
    svdpcv      = (void *)calloc(*nsvd,sizeof(double));
    ak          = (void *)calloc(lab,sizeof(double));
    bk          = (void *)calloc(lab,sizeof(double));
    svdpcv_tmp  = (double*)svdpcv;
    ak_tmp      = (double *)ak;
    bk_tmp      = (double *)bk;
    if( svdpcv == NULL || ak == NULL || bk == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for output values");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for work array. Note that this work array is actually
 * serving as a work array, plus memory for 6 other arrays in the 
 * Fortran routine (CRV, U, VT, SV, AK, BK).
 *
 * Originally we had "5*min(ncolx,ncoly)-4", but the SGI version of 
 * the DGESVD routine recommends "5*min(ncolx,ncoly)", so we decided
 * to take the bigger of the two, and go with "5*min(ncolx,ncoly)".
 */
  lwk = ncolx*ncoly + nsvmx*ncolx + nsvmx*ncoly +
        ntimes*ncolx + ntimes*ncoly + nsvmx +
        max(3*nsvmx + max(ncolx,ncoly), 5*nsvmx);
  w = (double *)calloc(lwk,sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  if((ntimes <= INT_MAX) &&
     (ncolx <= INT_MAX) &&
     (ncoly <= INT_MAX) &&
     (lab <= INT_MAX) &&
     (lwk <= INT_MAX))
  {
    int intimes = (int) ntimes;
    int incolx = (int) ncolx;
    int incoly = (int) ncoly;
    int ilab = (int) lab;
    int ilwk = (int) lwk;
    NGCALLF(dsvdlap,DSVDLAP)(dx,dy,&intimes,&intimes,&incolx,&incoly,nsvd,&iflag,
                             &missing_dx.doubleval,&iprint,w,&ilwk,svdpcv_tmp,
                             homlft_tmp,homrgt_tmp,hetlft_tmp,hetrgt_tmp,
                             ak_tmp,bk_tmp,&ilab,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }

  if (ier) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"svdstd: ier = %d\n", ier );
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

/*
 * If hom/het were originally float, then we need to coerce them from
 * double to float.
 */
  if(type_homlft == NCL_float) {
    coerce_output_float_only(homlft,homlft_tmp,total_size_lft,0);
    NclFree(homlft_tmp);
  }

  if(type_hetlft == NCL_float) {
    coerce_output_float_only(hetlft,hetlft_tmp,total_size_lft,0);
    NclFree(hetlft_tmp);
  }

  if(type_homrgt == NCL_float) {
    coerce_output_float_only(homrgt,homrgt_tmp,total_size_rgt,0);
    NclFree(homrgt_tmp);
  }

  if(type_hetrgt == NCL_float) {
    coerce_output_float_only(hetrgt,hetrgt_tmp,total_size_rgt,0);
    NclFree(hetrgt_tmp);
  }

/*
 *  Assign values for various attributes.
 */
  ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;

  if(type_svdpcv == NCL_float) {
    fnorm  = (void *)calloc(1,sizeof(float));
    condn  = (void *)calloc(1,sizeof(float));
    coerce_output_float_only(svdpcv,svdpcv_tmp,*nsvd,0);
    coerce_output_float_only(ak,ak_tmp,lab,0);
    coerce_output_float_only(bk,bk_tmp,lab,0);
    NclFree(svdpcv_tmp);
    NclFree(ak_tmp);
    NclFree(bk_tmp);
  }
  else {
    fnorm  = (void *)calloc(1,sizeof(double));
    condn  = (void *)calloc(1,sizeof(double));
  }
  coerce_output_float_or_double(fnorm,&w[ipt7],type_svdpcv,1,0);
  coerce_output_float_or_double(condn,&w[ipt7+1],type_svdpcv,1,0);
  lapack_err  = (int *)calloc(1,sizeof(int));
  *lapack_err = (int)w[ipt7+3];

/*
 * Set up variable to return.
 */
  type_svdpcv_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_svdpcv)));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            svdpcv,
                            NULL,
                            1,
                            dsizes_svdpcv,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_svdpcv_class
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Create individual attributes.
 */
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         fnorm,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "fnorm",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         condn,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "condn",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)lapack_err,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "lapack_err",
             att_md,
             NULL
             );

  dsizes[0] = lab;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         ak,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "ak",
             att_md,
             NULL
             );


  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         bk,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "bk",
             att_md,
             NULL
             );

  NclFree(w);

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


NhlErrorTypes svdcov_sv_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *svlft, *svrgt;
  double *dx, *dy, *svlft_tmp, *svrgt_tmp;
  ng_size_t dsizes_x[2];
  int has_missing_x;
  ng_size_t dsizes_y[2];
  int has_missing_y;
  ng_size_t dsizes_svlft[2];
  ng_size_t dsizes_svrgt[2];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_svlft, type_svrgt;
  ng_size_t ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt;
  ng_size_t lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv_tmp, *sv_tmp;
  void *svdpcv, *sv;
  ng_size_t dsizes_svdpcv[1];
  NclBasicDataTypes type_svdpcv;
  NclTypeClass type_svdpcv_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  ng_size_t total_size_x, total_size_y, total_size_svrgt, total_size_svlft;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           5,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           5,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
  svlft = (void*)NclGetArgValue(
           3,
           5,
           NULL,
           dsizes_svlft,
           NULL,
           NULL,
           &type_svlft,
           DONT_CARE);

  svrgt = (void*)NclGetArgValue(
           4,
           5,
           NULL,
           dsizes_svrgt,
           NULL,
           NULL,
           &type_svrgt,
           DONT_CARE);
/*
 * The rightmost dimensions must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The rightmost dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];

/*
 * Check dimension sizes.  The first dimension must be nsvd. The
 *  second dimension must be the same as the first dimension of
 * x (or y if it's the rgt arrays).
 */
  if( dsizes_svlft[0] != *nsvd || dsizes_svlft[1] != dsizes_x[0]) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The rightmost dimension of the left array must be the same as the leftmost dimension of x, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_svrgt[0] != *nsvd || 
      dsizes_svrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The rightmost dimension of the right array must be the same as the leftmost dimension of y, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Calculate total sizes of input arrays.
 */
  total_size_x     = ntimes * ncolx;
  total_size_y     = ntimes * ncoly;
  total_size_svlft =  *nsvd * ncolx;
  total_size_svrgt =  *nsvd * ncoly;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x and y to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_y,has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * svlft and svrgt must be float or double. It doesn't matter what the input
 * types are.
 */
  if((type_svlft != NCL_float && type_svlft != NCL_double) ||
     (type_svrgt != NCL_float && type_svrgt != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: svLeft/svRight must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision svrgt/lft. There's no need to do a
 * coercion because svrgt/lft is an output-only variable (i.e, there are no
 * values coming in).  svrgt/lft can only be float or double, so only
 * allocate space for a d.p. array if svrgt/lft is float.
 */
  svlft_tmp = coerce_output_double(svlft,type_svlft,total_size_svlft);
  svrgt_tmp = coerce_output_double(svrgt,type_svrgt,total_size_svrgt);
  if(svlft_tmp == NULL || svrgt_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Determine type of output values and allocate space for them.
 */
  dsizes_svdpcv[0] = *nsvd;
  nsvmx            = min(ncolx,ncoly);

  if(type_x != NCL_double && type_y != NCL_double) {
    type_svdpcv = NCL_float;
    svdpcv      = (void *)calloc(*nsvd,sizeof(float));
    svdpcv_tmp  = (double *)calloc(*nsvd,sizeof(double));
    sv          = (void *)calloc(nsvmx,sizeof(float));
    sv_tmp      = (double *)calloc(nsvmx,sizeof(double));
    if(svdpcv == NULL || svdpcv_tmp == NULL || sv == NULL || sv_tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_svdpcv = NCL_double;
    svdpcv      = (void *)calloc(*nsvd,sizeof(double));
    sv          = (void *)calloc(nsvmx,sizeof(double));
    svdpcv_tmp  = (double*)svdpcv;
    sv_tmp      = (double*)sv;
    if(svdpcv == NULL || sv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
/*
 * Allocate memory for work and output arrays.
 */
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*nsvmx);
  w     = (double *)calloc(lwork,sizeof(double));
  u     = (double *)calloc(nsvmx*ncolx,sizeof(double));
  vt    = (double *)calloc(nsvmx*ncoly,sizeof(double));
  crv   = (double *)calloc(ncolx*ncoly,sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  if((ntimes <= INT_MAX) &&
     (ncolx <= INT_MAX) &&
     (ncoly <= INT_MAX) &&
     (nsvmx <= INT_MAX) &&
     (lwork <= INT_MAX))
  {
    int intimes = (int) ntimes;
    int incolx = (int) ncolx;
    int incoly = (int) ncoly;
    int insvmx = (int) nsvmx;
    int ilwork = (int) lwork;
    NGCALLF(dsvdsv,DSVDSV)(dx,dy,&intimes,&intimes,&incolx,&incoly,nsvd,&iflag,
                           &missing_dx.doubleval,&iprint,svlft_tmp,svrgt_tmp,
                           svdpcv_tmp,crv,u,vt,sv_tmp,&insvmx,w,&ilwork,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
/*
 * Free memory.
 */
  NclFree(w);
  NclFree(crv);
  NclFree(u);
  NclFree(vt);

  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

/*
 * If svlft/rgt were originally float, then we need to coerce them from
 * double to float.
 */
  if(type_svrgt == NCL_float) {
    coerce_output_float_only(svrgt,svrgt_tmp,total_size_svrgt,0);
    NclFree(svrgt_tmp);
  }

  if(type_svlft == NCL_float) {
    coerce_output_float_only(svlft,svlft_tmp,total_size_svlft,0);
    NclFree(svlft_tmp);
  }

  if(type_svdpcv != NCL_double) {
    coerce_output_float_only(svdpcv,svdpcv_tmp,*nsvd,0);
    coerce_output_float_only(sv,sv_tmp,nsvmx,0);
    NclFree(sv_tmp);
    NclFree(svdpcv_tmp);
  }

/*
 * Set up return values.
 */
  type_svdpcv_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_svdpcv)));

  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            svdpcv,
                            NULL,
                            1,
                            dsizes_svdpcv,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_svdpcv_class
                            );

/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = nsvmx;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         sv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "sv",
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


NhlErrorTypes svdstd_sv_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *svlft, *svrgt;
  double *dx, *dy, *svlft_tmp, *svrgt_tmp;
  ng_size_t dsizes_x[2];
  int has_missing_x;
  ng_size_t dsizes_y[2];
  int has_missing_y;
  ng_size_t dsizes_svlft[2];
  ng_size_t dsizes_svrgt[2];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_svlft, type_svrgt;
  ng_size_t ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt;
  ng_size_t lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv_tmp, *sv_tmp;
  void *svdpcv, *sv;
  ng_size_t dsizes_svdpcv[1];
  NclBasicDataTypes type_svdpcv;
  NclTypeClass type_svdpcv_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  ng_size_t total_size_x, total_size_y, total_size_svrgt, total_size_svlft;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           5,
           NULL,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
  y = (void*)NclGetArgValue(
           1,
           5,
           NULL,
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           DONT_CARE);
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
  svlft = (void*)NclGetArgValue(
           3,
           5,
           NULL,
           dsizes_svlft,
           NULL,
           NULL,
           &type_svlft,
           DONT_CARE);

  svrgt = (void*)NclGetArgValue(
           4,
           5,
           NULL,
           dsizes_svrgt,
           NULL,
           NULL,
           &type_svrgt,
           DONT_CARE);
/*
 * The rightmost dimensions must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The rightmost dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];

/*
 * Check dimension sizes.  The first dimension must be nsvd. The
 *  second dimension must be the same as the first dimension of
 * x (or y if it's the rgt arrays).
 */
  if( dsizes_svlft[0] != *nsvd || dsizes_svlft[1] != dsizes_x[0]) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The rightmost dimension of the left array must be the same as the leftmost dimension of x, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_svrgt[0] != *nsvd || 
      dsizes_svrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The rightmost dimension of the right array must be the same as the leftmost dimension of y, and the leftmost dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Calculate total sizes of input arrays.
 */
  total_size_x     = ntimes * ncolx;
  total_size_y     = ntimes * ncoly;
  total_size_svlft =  *nsvd * ncolx;
  total_size_svrgt =  *nsvd * ncoly;
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,NULL);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,NULL);
/*
 * Coerce x and y to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  dy = coerce_input_double(y,type_y,total_size_y,has_missing_y,&missing_y,
                           &missing_dy);
  if(dx == NULL || dy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * svlft and svrgt must be float or double. It doesn't matter what the input
 * types are.
 */
  if(((type_svlft != NCL_float) && (type_svlft != NCL_double)) ||
     ((type_svrgt != NCL_float) && (type_svrgt != NCL_double))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: svLeft/svRight must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision svrgt/lft. There's no need to do a
 * coercion because svrgt/lft is an output-only variable (i.e, there are no
 * values coming in).  svrgt/lft can only be float or double, so only
 * allocate space for a d.p. array if svrgt/lft is float.
 */
  svlft_tmp = coerce_output_double(svlft,type_svlft,total_size_svlft);
  svrgt_tmp = coerce_output_double(svrgt,type_svrgt,total_size_svrgt);
  if(svlft_tmp == NULL || svrgt_tmp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Determine type of output values and allocate space for them.
 */
  dsizes_svdpcv[0] = *nsvd;
  nsvmx            = min(ncolx,ncoly);

  if(type_x != NCL_double && type_y != NCL_double) {
    type_svdpcv = NCL_float;
    svdpcv      = (void *)calloc(*nsvd,sizeof(float));
    svdpcv_tmp  = (double *)calloc(*nsvd,sizeof(double));
    sv          = (void *)calloc(nsvmx,sizeof(float));
    sv_tmp      = (double *)calloc(nsvmx,sizeof(double));
    if(svdpcv == NULL || svdpcv_tmp == NULL || sv == NULL || sv_tmp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }
  else {
    type_svdpcv = NCL_double;
    svdpcv      = (void *)calloc(*nsvd,sizeof(double));
    sv          = (void *)calloc(nsvmx,sizeof(double));
    svdpcv_tmp  = (double*)svdpcv;
    sv_tmp      = (double*)sv;
    if(svdpcv == NULL || sv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for work and output arrays.
 */
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*nsvmx);
  w     = (double *)calloc(lwork,sizeof(double));
  u     = (double *)calloc(nsvmx*ncolx,sizeof(double));
  vt    = (double *)calloc(nsvmx*ncoly,sizeof(double));
  crv   = (double *)calloc(ncolx*ncoly,sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */

  if((ntimes <= INT_MAX) &&
     (ncolx <= INT_MAX) &&
     (ncoly <= INT_MAX) &&
     (nsvmx <= INT_MAX) &&
     (lwork <= INT_MAX))
  {
    int intimes = (int) ntimes;
    int incolx = (int) ncolx;
    int incoly = (int) ncoly;
    int insvmx = (int) nsvmx;
    int ilwork = (int) lwork;
    NGCALLF(dsvdsv,DSVDSV)(dx,dy,&intimes,&intimes,&incolx,&incoly,nsvd,&iflag,
                           &missing_dx.doubleval,&iprint,svlft_tmp,svrgt_tmp,
                           svdpcv_tmp,crv,u,vt,sv_tmp,&insvmx,w,&ilwork,&ier);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
/*
 * Free memory.
 */
  NclFree(w);
  NclFree(crv);
  NclFree(u);
  NclFree(vt);

  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

/*
 * If svlft/rgt were originally float, then we need to coerce them from
 * double to float.
 */
  if(type_svrgt == NCL_float) {
    coerce_output_float_only(svrgt,svrgt_tmp,total_size_svrgt,0);
    NclFree(svrgt_tmp);
  }

  if(type_svlft == NCL_float) {
    coerce_output_float_only(svlft,svlft_tmp,total_size_svlft,0);
    NclFree(svlft_tmp);
  }

  if(type_svdpcv != NCL_double) {
    coerce_output_float_only(svdpcv,svdpcv_tmp,*nsvd,0);
    coerce_output_float_only(sv,sv_tmp,nsvmx,0);
    NclFree(sv_tmp);
    NclFree(svdpcv_tmp);
  }

/*
 * Set up return values.
 */
  type_svdpcv_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_svdpcv)));

  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            svdpcv,
                            NULL,
                            1,
                            dsizes_svdpcv,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_svdpcv_class
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = nsvmx;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         sv,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)type_svdpcv_class
                         );
  _NclAddAtt(
             att_id,
             "sv",
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


NhlErrorTypes svd_lapack_W( void )
{
/*
 * Input array variables
 */
  void *a, *u, *v;
  NrmQuark *jobu, *jobv;
  int *optv;
  double *tmp_a, *tmp_u, *tmp_vt;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_u[2];
  ng_size_t dsizes_v[2];
  int has_missing_a;
  NclScalar missing_a, missing_da;
  NclBasicDataTypes type_a, type_u, type_v;
/*
 * Work array variables
 */
  double *work;
  ng_size_t lwk;
/*
 * Output array variables
 */
  double *tmp_sgesvd;
  void *sgesvd;
  int *info;
  ng_size_t dsizes_sgesvd[1];
  NclBasicDataTypes type_sgesvd;
  NclTypeClass type_sgesvd_class;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  ng_size_t i, j, k1, k2, nrow, ncol, minrc, size_a, size_u, size_v;
  ng_size_t ldu, ucol, ldvt;
  int found_missing_a;
/*
 * Retrieve input parameters.
 */
  a = (void*)NclGetArgValue(
           0,
           6,
           &ndims_a, 
           dsizes_a,
           &missing_a,
           &has_missing_a,
           &type_a,
           DONT_CARE);

/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_a < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  jobu = (NrmQuark *)NclGetArgValue(
           1,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

  jobv = (NrmQuark *)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
/*
 * This part not needed yet, because "S" and "S" are being hard-coded
 * for jobu and jobv.
 *
 *  c_jobu = NrmQuarkToString(*jobu);
 *  c_jobv = NrmQuarkToString(*jobv);
 */

  optv = (int *)NclGetArgValue(
            3,
            6, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

  u = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           DONT_CARE);

  v = (void*)NclGetArgValue(
           5,
           6,
           NULL,
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           DONT_CARE);

/*
 * Check the dimensions of u and v against the dimensions of a.
 * The leftmost dimensions of "a" will be collapsed into one
 * dimension (ncol).
 */
  ncol = 1;
  for( i = 0; i <= ndims_a-2; i++ ) ncol *= dsizes_a[i];
  nrow  = dsizes_a[ndims_a-1];
  minrc = min(nrow,ncol);
  ucol  = dsizes_u[0];
  ldu   = dsizes_u[1];
  ldvt  = dsizes_v[1];

  if(ldu < nrow || ucol < minrc) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: The dimensions of 'u' must be min(N,M) x M, where (N,M) are the dimensions of 'a'");
    return(NhlFATAL);
  }

  if( dsizes_v[0] != ncol || ldvt < minrc) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: The dimensions of 'v' must be min(N,M) x N where (N,M) are the dimensions of 'a'");
    return(NhlFATAL);
  }

/*
 * The hom/het arrays must be float or double. It doesn't matter what
 * the input types are.
 */
  if(((type_u != NCL_float) && (type_u != NCL_double)) ||
     ((type_v != NCL_float) && (type_v != NCL_double))) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: The u, v arrays must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Calculate total sizes of input arrays and output arrays.
 */
  size_a = ncol * nrow;
  size_u = ucol * ldu;
  size_v = ncol * ldvt;
/*
 * Coerce input to double if necessary
 */
  tmp_a = coerce_input_double(a,type_a,size_a,has_missing_a,&missing_a,
                              &missing_da);
/*
 * Check for missing values, which are not allowed.
 */
    found_missing_a = contains_missing(tmp_a,size_a,has_missing_a,
                                       missing_da.doubleval);
    if(found_missing_a) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: The input array cannot contain any missing values");
      set_subset_output_missing(a,0,type_a,size_a,missing_da.doubleval);
      return(NhlFATAL);
    }
/*
 * Allocate space for double precision u/v. There's no need to do a
 * coercion because u/v are output-only variables (i.e, there are no
 * values coming in).  u/v can only be float or double, so only
 * allocate space for a dp array if u/v is float.
 *
 * If v needs to be transposed, (optv=1), then create space for tmp_vt
 * no matter what.
 */
  info  = (int*)calloc(1,sizeof(int));
  tmp_u = coerce_output_double(u,type_u,size_u);
  if(*optv == 1) {
    tmp_vt = (double*)calloc(size_v,sizeof(double));
  }
  else {
    tmp_vt = coerce_output_double(v,type_v,size_v);
  }
  if(tmp_u == NULL || tmp_vt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
  }

/*
 * Determine type of output values and allocate space for them.
 */
  dsizes_sgesvd[0] = minrc;
  if(type_a != NCL_double) {
    type_sgesvd = NCL_float;
    sgesvd      = (void *)calloc(minrc,sizeof(float));
    tmp_sgesvd  = (double *)calloc(minrc,sizeof(double));

    if( sgesvd == NULL || tmp_sgesvd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_sgesvd = NCL_double;
    sgesvd      = (void *)calloc(minrc,sizeof(double));
    if( sgesvd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_sgesvd  = (double *)sgesvd;
  }

/*
 * Allocate memory for work array.
 */
  lwk  = max( 3*minrc + max(nrow,ncol), 5 * minrc);
  work = (double *)calloc(lwk,sizeof(double));
  if( work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */

  if((nrow <= INT_MAX) &&
     (ncol <= INT_MAX) &&
     (ldu <= INT_MAX) &&
     (ldvt <= INT_MAX) &&
     (lwk <= INT_MAX))
  {
    int inrow = (int) nrow;
    int incol = (int) ncol;
    int ildu = (int) ldu;
    int ildvt = (int) ldvt;
    int ilwk = (int) lwk;
    NGCALLF(dgesvd,DGESVD)("S", "S", &inrow, &incol, tmp_a, &inrow, 
                           tmp_sgesvd, tmp_u, &ildu, tmp_vt, &ildvt, 
                           work, &ilwk, info);
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svd_lapack: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }

/*
 * Coerce output back to float if necessary.
 */
  if(type_sgesvd == NCL_float) {
    coerce_output_float_only(sgesvd,tmp_sgesvd,minrc,0);
    NclFree(tmp_sgesvd);
  }
/* 
 * Coerce u and v back to float if necessary and transpose v if desired.
 */
  if(type_u == NCL_float) {
    coerce_output_float_only(u,tmp_u,size_u,0);
    NclFree(tmp_u);
  }

  if(*optv == 1) {
    if(type_v == NCL_float) {
      for( i = 0; i < ncol; i++ ) {
        for( j = 0; j < ldvt; j++ ) {
          k1 = i * ncol + j;
          k2 = j * ldvt + i;
          ((float*)v)[k2]  = (float)tmp_vt[k1];
        }
      }
    }
    else {
      for( i = 0; i < ncol; i++ ) {
        for( j = 0; j < ldvt; j++ ) {
          k1 = i * ncol + j;
          k2 = j * ldvt + i;
          ((double*)v)[k2]  = (double)tmp_vt[k1];
        }
      }
    }
    NclFree(tmp_vt);
  }
  else {
    if(type_v == NCL_float ) {
      coerce_output_float_only(v,tmp_vt,size_v,0);
      NclFree(tmp_vt);
    }
  }

/*
 * Free memory.
 */
  NclFree(work);

  if(type_a != NCL_double)
      NclFree(tmp_a);

/*
 * Set up variable to return and assign values for "info" attribute.
 */
  type_sgesvd_class = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_sgesvd)));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            sgesvd,
                            NULL,
                            1,
                            dsizes_sgesvd,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_sgesvd_class
                            );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Create individual attributes.
 */
  dsizes[0] = 1;

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)info,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "info",
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



NhlErrorTypes svdpar_W( void )
{
/*
 * Input variables.
 */
  void *x;
  double *dx;
  ng_size_t dsizes_x[2];
  NclBasicDataTypes type_x;
  NrmQuark *label;
  char *label2;
/*
 * Get data to print.
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);
/*
 * Coerce to double if necessary.
 */
  dx = coerce_input_double(x,type_x,dsizes_x[0]*dsizes_x[1],0,NULL,NULL);

/*
 * Get label.
 */
  label = (NrmQuark *)NclGetArgValue(
            1,
            2,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
  label2 = NrmQuarkToString(*label);
/*
 * Call the Fortran version of 'dsvdpar' with the full argument list.
 */

  if((dsizes_x[0] <= INT_MAX) && (dsizes_x[1] <= INT_MAX))
  {
    int x0 = (int) dsizes_x[0];
    int x1 = (int) dsizes_x[1];
    NGCALLF(dsvdpar,DSVDPAR)(dx,&x1,&x1,&x0,
                             label2,strlen(label2));
  }
  else
  {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdpar: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
/*
 * Free up memory and return.
 */
  if((void*)dx != x) NclFree(dx);
  return(NhlNOERROR);
}

#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <math.h>
#include <ncarg/gks.h>

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dsvdlap,DSVDLAP)(double *,double *,int *,int *,int *,
                                     int *,int *,int *,double  *,int *,
                                     double *,int *,double *, double *,
                                     double *, double *, double *,int *);

extern void NGCALLF(dsvdsv,DSVDSV)(double *,double *, int *,int *,int *,
                                   int *,int *,int *,double *, int *,
                                   double *, double *,double *, double *,
                                   double *,double *, double *,int *,
                                   double *,int *,int *);

NhlErrorTypes svdcov_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *homlft, *hetlft, *homrgt, *hetrgt;
  double *dx, *dy, *dhomlft, *dhetlft, *dhomrgt, *dhetrgt;
  float *rhomlft, *rhetlft, *rhomrgt, *rhetrgt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  int ndims_homlft, dsizes_homlft[NCL_MAX_DIMENSIONS];
  int ndims_hetlft, dsizes_hetlft[NCL_MAX_DIMENSIONS];
  int ndims_homrgt, dsizes_homrgt[NCL_MAX_DIMENSIONS];
  int ndims_hetrgt, dsizes_hetrgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  NclBasicDataTypes type_homlft, type_hetlft, type_homrgt, type_hetrgt;
  int ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w;
  int lwk;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv;
  double *fnorm, *condn;
  float *rfnorm, *rcondn;
  int *lapack_err, ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  int i, j, ipt7, any_double = 0;
  int total_size_x, total_size_y, total_size_lft, total_size_rgt;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           7,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           7,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);

  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  homlft = (void*)NclGetArgValue(
           3,
           7,
           &ndims_homlft, 
           dsizes_homlft,
           NULL,
           NULL,
           &type_homlft,
           2);

  hetlft = (void*)NclGetArgValue(
           4,
           7,
           &ndims_hetlft, 
           dsizes_hetlft,
           NULL,
           NULL,
           &type_hetlft,
           2);


  homrgt = (void*)NclGetArgValue(
           5,
           7,
           &ndims_homrgt, 
           dsizes_homrgt,
           NULL,
           NULL,
           &type_homrgt,
           2);

  hetrgt = (void*)NclGetArgValue(
           6,
           7,
           &ndims_hetrgt, 
           dsizes_hetrgt,
           NULL,
           NULL,
           &type_hetrgt,
           2);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];

/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.  The first dimension must be nsvd. The second
 * dimension must be the same as the first dimension of x (or y if
 * it's the rgt arrays).
 */
  if( dsizes_homlft[0] != *nsvd || 
      dsizes_hetlft[0] != *nsvd ||
      dsizes_homlft[1] != dsizes_x[0] || 
      dsizes_hetlft[1] != dsizes_x[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The second dimension of the homlft/hetlft arrays must be the same as the first dimension of x, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_homrgt[0] != *nsvd || dsizes_hetrgt[0] != *nsvd ||
      dsizes_homrgt[1] != dsizes_y[0] ||
      dsizes_hetrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The second dimension of the homrgt/hetrgt arrays must be the same as the first dimension of y, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * The hom/het arrays must be float or double. It doesn't matter what
 * the input types are.
 */
  if(type_homlft != NCL_float && type_homlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The homlft array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_hetlft != NCL_float && type_hetlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The hetlft array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_homrgt != NCL_float && type_homrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The homrgt array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_hetrgt != NCL_float && type_hetrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: The hetrgt array must be of type float or double");
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
 * Coerce x missing value to double.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce y missing value to double.
 */
  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
  }
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
    any_double = 1;
/*
 * x is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
    any_double = 1;
/*
 * y is already double.
 */
    dy = (double*)y;
  }


/*
 * Allocate space for double precision hom/het. There's no need to do a
 * coercion because hom/het is an output-only variable (i.e, there are no
 * values coming in).  hom/het can only be float or double, so only
 * allocate space for a d.p. array if hom/het is float.
 */
  if(type_homlft != NCL_double) {
    dhomlft = (double*)NclMalloc(sizeof(double)*total_size_lft);
    if( dhomlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing homlft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * homlft is already double.
 */
    dhomlft = (double*)homlft;
  }

/*
 * Coerce hetlft to double if necessary.
 */
  if(type_hetlft != NCL_double) {
    dhetlft = (double*)NclMalloc(sizeof(double)*total_size_lft);
    if( dhetlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing hetlft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * hetlft is already double.
 */
    dhetlft = (double*)hetlft;
  }

/*
 * Coerce homrgt to double if necessary.
 */
  if(type_homrgt != NCL_double) {
    dhomrgt = (double*)NclMalloc(sizeof(double)*total_size_rgt);
    if( dhomrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing homrgt array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * homrgt is already double.
 */
    dhomrgt = (double*)homrgt;
  }

/*
 * Coerce hetrgt to double if necessary.
 */
  if(type_hetrgt != NCL_double) {
    dhetrgt = (double*)NclMalloc(sizeof(double)*total_size_rgt);
    if( dhetrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for coercing hetrgt array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * hetrgt is already double.
 */
    dhetrgt = (double*)hetrgt;
  }

/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwk = ncolx*ncoly + ncolx*ncolx + ncoly*ncoly +
        ntimes*ncolx + ntimes*ncoly + min(ncolx,ncoly) +
        max(3*min(ncolx,ncoly) + max(ncolx,ncoly), 5*min(ncolx,ncoly)-4);
  w = (double *)NclMalloc(lwk*sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdlap,DSVDLAP)(dx,dy,&ntimes,&ntimes,&ncolx,&ncoly,nsvd,
                           &iflag,&missing_dx.doubleval,&iprint,w,&lwk,svdpcv,
                           dhomlft,dhomrgt,dhetlft,dhetrgt,&ier);
  if (ier) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"svdcov: ier = %d\n", ier );
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

  NclFree(w);
/*
 * If hom/het were originally float, then we need to coerce them from
 * double to float. Do this by creating a pointer of type float that 
 * points to the original location, and then loop through the values and
 * do the coercion.
 */
  if(type_homlft == NCL_float) {
    rhomlft = (float*)homlft;     /* Float pointer to original homlft array */
    for( i = 0; i < total_size_lft; i++ ) rhomlft[i]  = (float)dhomlft[i];
    NclFree(dhomlft);   /* Free up the double array */
  }

  if(type_hetlft == NCL_float) {
    rhetlft = (float*)hetlft;     /* Float pointer to original hetlft array */
    for( i = 0; i < total_size_lft; i++ ) rhetlft[i]  = (float)dhetlft[i];
    NclFree(dhetlft);   /* Free up the double array */
  }

  if(type_homrgt == NCL_float) {
    rhomrgt = (float*)homrgt;     /* Float pointer to original homrgt array */
    for( i = 0; i < total_size_rgt; i++ ) rhomrgt[i]  = (float)dhomrgt[i];
    NclFree(dhomrgt);   /* Free up the double array */
  }

  if(type_hetrgt == NCL_float) {
    rhetrgt = (float*)hetrgt;     /* Float pointer to original hetrgt array */
    for( i = 0; i < total_size_rgt; i++ ) rhetrgt[i]  = (float)dhetrgt[i];
    NclFree(dhetrgt);   /* Free up the double array */
  }

/*
 * Return output grid to NCL.
 */
  if(!any_double) {
/*
 * Copy double values to float values.
 */
    rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
    if( rsvdpcv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
/*
 * Free up memory.
 */
    NclFree(svdpcv);

/*
 * Set up variable to return.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsvdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Get attribute values.
 */
    rfnorm     = (float *)NclMalloc(sizeof(float));
    rcondn     = (float *)NclMalloc(sizeof(float));
    lapack_err = (int *)NclMalloc(sizeof(int));
    if( rfnorm == NULL || rcondn == NULL || lapack_err == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }
/*
 * Get attribute values and coerce to float.
 */
    ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
    *rfnorm     = (float)w[ipt7];
    *rcondn     = (float)w[ipt7+1];
    *lapack_err = (int)w[ipt7+3];

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rfnorm,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           (void*)rcondn,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
    
  }
  else {
/*
 * One or more input arrays were double, so return doubles.
 *
 * Set up variable to return.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)svdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Set some attribute values.
 *
 * First allocate memory for them.
 */
    fnorm      = (double *)NclMalloc(sizeof(double));
    condn      = (double *)NclMalloc(sizeof(double));
    lapack_err = (int *)NclMalloc(sizeof(int));
    if( fnorm == NULL || condn == NULL || lapack_err == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }

    ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
    *fnorm      = w[ipt7];
    *condn      = w[ipt7+1];
    *lapack_err = (int)w[ipt7+3];

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)fnorm,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
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
                           (void*)condn,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
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
  }
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
  double *dx, *dy, *dhomlft, *dhetlft, *dhomrgt, *dhetrgt;
  float *rhomlft, *rhetlft, *rhomrgt, *rhetrgt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  int ndims_homlft, dsizes_homlft[NCL_MAX_DIMENSIONS];
  int ndims_hetlft, dsizes_hetlft[NCL_MAX_DIMENSIONS];
  int ndims_homrgt, dsizes_homrgt[NCL_MAX_DIMENSIONS];
  int ndims_hetrgt, dsizes_hetrgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y;
  NclBasicDataTypes type_homlft, type_hetlft, type_homrgt, type_hetrgt;
  int ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w;
  int lwk;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv;
  double *fnorm, *condn;
  float *rfnorm, *rcondn;
  int *lapack_err, ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * various
 */
  int i, j, ipt7, any_double = 0;
  int total_size_x, total_size_y, total_size_lft, total_size_rgt;
/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           7,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           7,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);

  nsvd = (int *)NclGetArgValue(
            2,
            7, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  homlft = (void*)NclGetArgValue(
           3,
           7,
           &ndims_homlft, 
           dsizes_homlft,
           NULL,
           NULL,
           &type_homlft,
           2);

  hetlft = (void*)NclGetArgValue(
           4,
           7,
           &ndims_hetlft, 
           dsizes_hetlft,
           NULL,
           NULL,
           &type_hetlft,
           2);


  homrgt = (void*)NclGetArgValue(
           5,
           7,
           &ndims_homrgt, 
           dsizes_homrgt,
           NULL,
           NULL,
           &type_homrgt,
           2);

  hetrgt = (void*)NclGetArgValue(
           6,
           7,
           &ndims_hetrgt, 
           dsizes_hetrgt,
           NULL,
           NULL,
           &type_hetrgt,
           2);
/*
 * The last dimension of x and y must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The last dimension of both input arrays must be the same size");
    return(NhlFATAL);
  }
  ncolx  = dsizes_x[0];
  ncoly  = dsizes_y[0];
  ntimes = dsizes_x[1];

/*
 * Check nsvd, the number of SVD patterns to be calculated.
 */
  if (*nsvd > min(ncolx,ncoly)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: nsvx exceeds maximum possible patterns");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.  The first dimension must be nsvd. The second
 * dimension must be the same as the first dimension of x (or y if
 * it's the rgt arrays).
 */
  if( dsizes_homlft[0] != *nsvd || 
      dsizes_hetlft[0] != *nsvd ||
      dsizes_homlft[1] != dsizes_x[0] || 
      dsizes_hetlft[1] != dsizes_x[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The second dimension of the homlft/hetlft arrays must be the same as the first dimension of x, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_homrgt[0] != *nsvd || dsizes_hetrgt[0] != *nsvd ||
      dsizes_homrgt[1] != dsizes_y[0] ||
      dsizes_hetrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The second dimension of the homrgt/hetrgt arrays must be the same as the first dimension of y, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
/*
 * The hom/het arrays must be float or double. It doesn't matter what
 * the input types are.
 */
  if(type_homlft != NCL_float && type_homlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The homlft array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_hetlft != NCL_float && type_hetlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The hetlft array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_homrgt != NCL_float && type_homrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The homrgt array must be of type float or double");
    return(NhlFATAL);
  }

  if(type_hetrgt != NCL_float && type_hetrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: The hetrgt array must be of type float or double");
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
 * Coerce x missing value to double.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce y missing value to double.
 */
  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
  }
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
    any_double = 1;
/*
 * x is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
    any_double = 1;
/*
 * y is already double.
 */
    dy = (double*)y;
  }


/*
 * Allocate space for double precision hom/het. There's no need to do a
 * coercion because hom/het is an output-only variable (i.e, there are no
 * values coming in).  hom/het can only be float or double, so only
 * allocate space for a d.p. array if hom/het is float.
 */
  if(type_homlft != NCL_double) {
    dhomlft = (double*)NclMalloc(sizeof(double)*total_size_lft);
    if( dhomlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing homlft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * homlft is already double.
 */
    dhomlft = (double*)homlft;
  }

/*
 * Coerce hetlft to double if necessary.
 */
  if(type_hetlft != NCL_double) {
    dhetlft = (double*)NclMalloc(sizeof(double)*total_size_lft);
    if( dhetlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing hetlft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * hetlft is already double.
 */
    dhetlft = (double*)hetlft;
  }

/*
 * Coerce homrgt to double if necessary.
 */
  if(type_homrgt != NCL_double) {
    dhomrgt = (double*)NclMalloc(sizeof(double)*total_size_rgt);
    if( dhomrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing homrgt array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * homrgt is already double.
 */
    dhomrgt = (double*)homrgt;
  }

/*
 * Coerce hetrgt to double if necessary.
 */
  if(type_hetrgt != NCL_double) {
    dhetrgt = (double*)NclMalloc(sizeof(double)*total_size_rgt);
    if( dhetrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for coercing hetrgt array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * hetrgt is already double.
 */
    dhetrgt = (double*)hetrgt;
  }

/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work array.
 */
  lwk = ncolx*ncoly + ncolx*ncolx + ncoly*ncoly +
        ntimes*ncolx + ntimes*ncoly + min(ncolx,ncoly) +
        max(3*min(ncolx,ncoly) + max(ncolx,ncoly), 5*min(ncolx,ncoly)-4);
  w = (double *)NclMalloc(lwk*sizeof(double));
  if( w == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdlap,DSVDLAP)(dx,dy,&ntimes,&ntimes,&ncolx,&ncoly,nsvd,
                           &iflag,&missing_dx.doubleval,&iprint,w,&lwk,svdpcv,
                           dhomlft,dhomrgt,dhetlft,dhetrgt,&ier);
  if (ier) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"svdstd: ier = %d\n", ier );
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)dy != y) NclFree(dy);

  NclFree(w);
/*
 * If hom/het were originally float, then we need to coerce them from
 * double to float. Do this by creating a pointer of type float that 
 * points to the original location, and then loop through the values and
 * do the coercion.
 */
  if(type_homlft == NCL_float) {
    rhomlft = (float*)homlft;     /* Float pointer to original homlft array */
    for( i = 0; i < total_size_lft; i++ ) rhomlft[i]  = (float)dhomlft[i];
    NclFree(dhomlft);   /* Free up the double array */
  }

  if(type_hetlft == NCL_float) {
    rhetlft = (float*)hetlft;     /* Float pointer to original hetlft array */
    for( i = 0; i < total_size_lft; i++ ) rhetlft[i]  = (float)dhetlft[i];
    NclFree(dhetlft);   /* Free up the double array */
  }

  if(type_homrgt == NCL_float) {
    rhomrgt = (float*)homrgt;     /* Float pointer to original homrgt array */
    for( i = 0; i < total_size_rgt; i++ ) rhomrgt[i]  = (float)dhomrgt[i];
    NclFree(dhomrgt);   /* Free up the double array */
  }

  if(type_hetrgt == NCL_float) {
    rhetrgt = (float*)hetrgt;     /* Float pointer to original hetrgt array */
    for( i = 0; i < total_size_rgt; i++ ) rhetrgt[i]  = (float)dhetrgt[i];
    NclFree(dhetrgt);   /* Free up the double array */
  }

/*
 * Return output grid to NCL.
 */
  if(!any_double) {
/*
 * Copy double values to float values.
 */
    rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
    if( rsvdpcv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
/*
 * Free up memory.
 */
    NclFree(svdpcv);

/*
 * Set up variable to return.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsvdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Get attribute values.
 */
    rfnorm     = (float *)NclMalloc(sizeof(float));
    rcondn     = (float *)NclMalloc(sizeof(float));
    lapack_err = (int *)NclMalloc(sizeof(int));
    if( rfnorm == NULL || rcondn == NULL || lapack_err == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }
/*
 * Get attribute values and coerce to float.
 */
    ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
    *rfnorm     = (float)w[ipt7];
    *rcondn     = (float)w[ipt7+1];
    *lapack_err = (int)w[ipt7+3];

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rfnorm,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
                           (void*)rcondn,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
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
    
  }
  else {
/*
 * One or more input arrays were double, so return doubles.
 *
 * Set up variable to return.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)svdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Set some attribute values.
 *
 * First allocate memory for them.
 */
    fnorm      = (double *)NclMalloc(sizeof(double));
    condn      = (double *)NclMalloc(sizeof(double));
    lapack_err = (int *)NclMalloc(sizeof(int));
    if( fnorm == NULL || condn == NULL || lapack_err == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }

    ipt7 = ncolx*ncoly + *nsvd*(ncolx+ncoly) + ntimes*(ncolx+ncoly) + *nsvd;
    *fnorm      = w[ipt7];
    *condn      = w[ipt7+1];
    *lapack_err = (int)w[ipt7+3];

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)fnorm,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
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
                           (void*)condn,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
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
  }
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
  double *dx, *dy, *dsvlft, *dsvrgt;
  float *rsvlft, *rsvrgt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS],has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  int ndims_svlft, dsizes_svlft[NCL_MAX_DIMENSIONS];
  int ndims_svrgt, dsizes_svrgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_svlft, type_svrgt;
  int ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=0, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt, *sv;
  int lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv, *rsv;
  int ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  int i, j, k, l, any_double = 0;
  int total_size_x, total_size_y, total_size_svrgt, total_size_svlft;

/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           5,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           5,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  svlft = (void*)NclGetArgValue(
           3,
           5,
           &ndims_svlft, 
           dsizes_svlft,
           NULL,
           NULL,
           &type_svlft,
           2);

  svrgt = (void*)NclGetArgValue(
           4,
           5,
           &ndims_svrgt, 
           dsizes_svrgt,
           NULL,
           NULL,
           &type_svrgt,
           2);
/*
 * The rightmost dimensions must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The last dimension of both input arrays must be the same size");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The second dimension of the left array must be the same as the first dimension of x, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_svrgt[0] != *nsvd || 
      dsizes_svrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: The second dimension of the right array must be the same as the first dimension of y, and the first dimension must be nsvx");
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
 * Coerce x missing value to double.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce y missing value to double.
 */
  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
  }
/*
 * Calculate total sizes of input arrays.
 */
  total_size_x     = ntimes * ncolx;
  total_size_y     = ntimes * ncoly;
  total_size_svlft =  *nsvd * ncolx;
  total_size_svrgt =  *nsvd * ncoly;
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
    any_double = 1;
/*
 * x is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
    any_double = 1;
/*
 * y is already double.
 */
    dy = (double*)y;
  }
/*
 * svlft and svrgt must be float or double. It doesn't matter what the input
 * types are.
 */
  if(type_svlft != NCL_float && type_svlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: svLeft must be of type float or double");
    return(NhlFATAL);
  }

  if(type_svrgt != NCL_float && type_svrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: svRight must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision svrgt/lft. There's no need to do a
 * coercion because svrgt/lft is an output-only variable (i.e, there are no
 * values coming in).  svrgt/lft can only be float or double, so only
 * allocate space for a d.p. array if svrgt/lft is float.
 */
  if(type_svlft != NCL_double) {
    dsvlft = (double*)NclMalloc(sizeof(double)*total_size_svlft);
    if( dsvlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing svLeft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * svlft is already double.
 */
    dsvlft = (double*)svlft;
  }

  if(type_svrgt != NCL_double) {
    dsvrgt = (double*)NclMalloc(sizeof(double)*total_size_svrgt);
    if( dsvrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for coercing svRight array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * svrgt is already double.
 */
    dsvrgt = (double*)svrgt;
  }

/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work and output arrays.
 */
  nsvmx = min(ncolx,ncoly);
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*min(ncolx,ncoly)-4);
  w   = (double *)NclMalloc(lwork*sizeof(double));
  u   = (double *)NclMalloc(nsvmx*ncolx*sizeof(double));
  vt  = (double *)NclMalloc(nsvmx*ncoly*sizeof(double));
  sv  = (double *)NclMalloc(nsvmx*sizeof(double));
  crv = (double *)NclMalloc(ncolx*ncoly*sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdsv,DSVDSV)(dx,dy,&ntimes,&ntimes,&ncolx,&ncoly,nsvd,&iflag,
                         &missing_dx.doubleval,&iprint,dsvlft,dsvrgt,
                         svdpcv,crv,u,vt,sv,&nsvmx,w,&lwork,&ier);
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
 * double to float.Do this by creating a pointer of type float that 
 * points to the original location, and then loop through the values and
 * do the coercion.
 */
  if(type_svrgt == NCL_float) {
    rsvrgt = (float*)svrgt;     /* Float pointer to original svrgt array */
    for( i = 0; i < total_size_svrgt; i++ ) rsvrgt[i]  = (float)dsvrgt[i];
    NclFree(dsvrgt);   /* Free up the double array */
  }

  if(type_svlft == NCL_float) {
    rsvlft = (float*)svlft;     /* Float pointer to original svlft array */
    for( i = 0; i < total_size_svlft; i++ ) rsvlft[i]  = (float)dsvlft[i];
    NclFree(dsvlft);   /* Free up the double array */
  }

/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * None of the input is double, so return floats.
 *
 * First copy double values to float values.
 */
    rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
    for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
    NclFree(svdpcv);

    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsvdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Convert doubles to floats.
 */
    rsv = (float *)NclMalloc(nsvmx*sizeof(float));
    if( rsv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdcov_sv: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }
        
    for(i = 0; i < nsvmx; i++) rsv[i] = (float)sv[i];
    NclFree(sv);

    dsizes[0] = nsvmx;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rsv,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "sv",
               att_md,
               NULL
               );

  }

  else {
/*
 * x and/or y is double, so return double values.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)svdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
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
                           (void*)sv,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "sv",
               att_md,
               NULL
               );
  }
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
  double *dx, *dy, *dsvlft, *dsvrgt;
  float *rsvlft, *rsvrgt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS],has_missing_x;
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  int ndims_svlft, dsizes_svlft[NCL_MAX_DIMENSIONS];
  int ndims_svrgt, dsizes_svrgt[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_dx, missing_dy;
  NclBasicDataTypes type_x, type_y, type_svlft, type_svrgt;
  int ntimes, ncolx, ncoly;
  int *nsvd;
  int ier = 0, iflag=1, iprint=0;
/*
 * Work array variables
 */
  double *w, *crv, *u, *vt, *sv;
  int lwork, nsvmx;
/*
 * Output array variables
 */
  double *svdpcv;
  float *rsvdpcv, *rsv;
  int ndims_svdpcv, dsizes_svdpcv[1];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[NCL_MAX_DIMENSIONS];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Various
 */
  int i, j, k, l, any_double = 0;
  int total_size_x, total_size_y, total_size_svrgt, total_size_svlft;

/*
 * Retrieve input parameters.
 */
  x = (void*)NclGetArgValue(
           0,
           5,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           2);
  y = (void*)NclGetArgValue(
           1,
           5,
           &ndims_y, 
           dsizes_y,
           &missing_y,
           &has_missing_y,
           &type_y,
           2);
  nsvd = (int *)NclGetArgValue(
            2,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            2);
  svlft = (void*)NclGetArgValue(
           3,
           5,
           &ndims_svlft, 
           dsizes_svlft,
           NULL,
           NULL,
           &type_svlft,
           2);

  svrgt = (void*)NclGetArgValue(
           4,
           5,
           &ndims_svrgt, 
           dsizes_svrgt,
           NULL,
           NULL,
           &type_svrgt,
           2);
/*
 * The rightmost dimensions must be the same.
 */
  if( dsizes_x[1] != dsizes_y[1] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The last dimension of both input arrays must be the same size");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The second dimension of the left array must be the same as the first dimension of x, and the first dimension must be nsvx");
    return(NhlFATAL);
  }
  if( dsizes_svrgt[0] != *nsvd || 
      dsizes_svrgt[1] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: The second dimension of the right array must be the same as the first dimension of y, and the first dimension must be nsvx");
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
 * Coerce x missing value to double.
 */
  if(has_missing_x) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dx,
               &missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_x != NCL_double) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
/*
 * Coerce y missing value to double.
 */
  if(has_missing_y) {
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               &missing_dy,
               &missing_y,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
  }
  else {
/*
 * Get the default missing value.
 */ 
    if(type_y != NCL_double) {
      missing_dy.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
  }
/*
 * Calculate total sizes of input arrays.
 */
  total_size_x     = ntimes * ncolx;
  total_size_y     = ntimes * ncoly;
  total_size_svlft =  *nsvd * ncolx;
  total_size_svrgt =  *nsvd * ncoly;
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)NclMalloc(sizeof(double)*total_size_x);
    if( dx == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing x array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 &missing_dx,
                 &missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dx,
                 x,
                 total_size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
    any_double = 1;
/*
 * x is already double.
 */
    dx = (double*)x;
  }

/*
 * Coerce y to double if necessary.
 */
  if(type_y != NCL_double) {
    dy = (double*)NclMalloc(sizeof(double)*total_size_y);
    if( dy == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing y array to double precision");
      return(NhlFATAL);
    }
    if(has_missing_y) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 &missing_dy,
                 &missing_y,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 dy,
                 y,
                 total_size_y,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_y)));
    }
  }
  else {
    any_double = 1;
/*
 * y is already double.
 */
    dy = (double*)y;
  }
/*
 * svlft and svrgt must be float or double. It doesn't matter what the input
 * types are.
 */
  if(type_svlft != NCL_float && type_svlft != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: svLeft must be of type float or double");
    return(NhlFATAL);
  }

  if(type_svrgt != NCL_float && type_svrgt != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: svRight must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Allocate space for double precision svrgt/lft. There's no need to do a
 * coercion because svrgt/lft is an output-only variable (i.e, there are no
 * values coming in).  svrgt/lft can only be float or double, so only
 * allocate space for a d.p. array if svrgt/lft is float.
 */
  if(type_svlft != NCL_double) {
    dsvlft = (double*)NclMalloc(sizeof(double)*total_size_svlft);
    if( dsvlft == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing svLeft array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * svlft is already double.
 */
    dsvlft = (double*)svlft;
  }

  if(type_svrgt != NCL_double) {
    dsvrgt = (double*)NclMalloc(sizeof(double)*total_size_svrgt);
    if( dsvrgt == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for coercing svRight array to double precision");
      return(NhlFATAL);
    }
  }
  else {
/*
 * svrgt is already double.
 */
    dsvrgt = (double*)svrgt;
  }

/*
 * Allocate space for output array.
 */
  dsizes_svdpcv[0] = *nsvd;
  svdpcv = (double *)NclMalloc(*nsvd*sizeof(double));
  if( svdpcv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for work and output arrays.
 */
  nsvmx = min(ncolx,ncoly);
  lwork = max(3*nsvmx+max(ncolx,ncoly),5*min(ncolx,ncoly)-4);
  w   = (double *)NclMalloc(lwork*sizeof(double));
  u   = (double *)NclMalloc(nsvmx*ncolx*sizeof(double));
  vt  = (double *)NclMalloc(nsvmx*ncoly*sizeof(double));
  sv  = (double *)NclMalloc(nsvmx*sizeof(double));
  crv = (double *)NclMalloc(ncolx*ncoly*sizeof(double));
  if( w == NULL || crv == NULL || u == NULL || vt == NULL || sv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran version of 'svdlap' with the full argument list.
 */
  NGCALLF(dsvdsv,DSVDSV)(dx,dy,&ntimes,&ntimes,&ncolx,&ncoly,nsvd,&iflag,
                         &missing_dx.doubleval,&iprint,dsvlft,dsvrgt,
                         svdpcv,crv,u,vt,sv,&nsvmx,w,&lwork,&ier);
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
 * double to float.Do this by creating a pointer of type float that 
 * points to the original location, and then loop through the values and
 * do the coercion.
 */
  if(type_svrgt == NCL_float) {
    rsvrgt = (float*)svrgt;     /* Float pointer to original svrgt array */
    for( i = 0; i < total_size_svrgt; i++ ) rsvrgt[i]  = (float)dsvrgt[i];
    NclFree(dsvrgt);   /* Free up the double array */
  }

  if(type_svlft == NCL_float) {
    rsvlft = (float*)svlft;     /* Float pointer to original svlft array */
    for( i = 0; i < total_size_svlft; i++ ) rsvlft[i]  = (float)dsvlft[i];
    NclFree(dsvlft);   /* Free up the double array */
  }

/*
 * Return values. 
 */
  if(type_x != NCL_double && type_y != NCL_double) {
/*
 * None of the input is double, so return floats.
 *
 * First copy double values to float values.
 */
    rsvdpcv = (float *)NclMalloc(*nsvd*sizeof(float));
    for( i = 0; i < *nsvd; i++ ) rsvdpcv[i] = (float)svdpcv[i];
    NclFree(svdpcv);

    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)rsvdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Convert doubles to floats.
 */
    rsv = (float *)NclMalloc(nsvmx*sizeof(float));
    if( rsv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"svdstd_sv: Unable to allocate memory for attributes");
      return(NhlFATAL);
    }
        
    for(i = 0; i < nsvmx; i++) rsv[i] = (float)sv[i];
    NclFree(sv);

    dsizes[0] = nsvmx;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rsv,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "sv",
               att_md,
               NULL
               );

  }

  else {
/*
 * x and/or y is double, so return double values.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)svdpcv,
                              NULL,
                              1,
                              dsizes_svdpcv,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
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
                           (void*)sv,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "sv",
               att_md,
               NULL
               );
  }
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

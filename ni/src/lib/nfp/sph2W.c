#include <stdio.h>
/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include
 */

#include "wrapper.h"

extern double pow(double,double);

extern void NGCALLF(dchkerr,DCHKERR)(char *,char *,int *,int *,int *,int
*,int,int);

extern void NGCALLF(ddivec,DDIVEC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *, int *,int *,double *,int *,double *,int *,int
*);

extern void NGCALLF(ddivgc,DDIVGC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *, int *,int *,double *,int *,double *,int *,int
*);

extern void NGCALLF(dgeomat,DGEOMAT)(int *,int *,double *,double *);

extern void NGCALLF(dgeomatv,DGEOMATV)(int *,int *,double *,double *,double *);

extern void NGCALLF(dgeoscl,DGEOSCL)(int *,int *,int *,double *,double *,int *);

extern void NGCALLF(dgradec,DGRADEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *,double *,int *,
double *,int *,int *);

extern void NGCALLF(dgradgc,DGRADGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *,double *,int *,
double *,int *,int *);

extern void NGCALLF(didivec,DIDIVEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *,double *,int *,
double *,int *,double *,int *);

extern void NGCALLF(didivgc,DIDIVGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *,double *,int *,
double *,int *,double *,int *);

extern void NGCALLF(didvtec,DIDVTEC)(int *,int *,int *,int *,double
*,double *,int *,int *, double *,double *,double *,double *,int *,int
*,double *,int *, double *,int *,double *,double *,int *);

extern void NGCALLF(didvtgc,DIDVTGC)(int *,int *,int *,int *,double
*,double *,int *,int *, double *,double *,double *,double *,int *,int
*,double *,int *,double *,int *,double *,double *,int *);

extern void NGCALLF(digradec,DIGRADEC)(int *,int *,int *,int *,double
*,int *,int *,double *,double *, int *,int *,double *,int *,double *,int
*,int *);

extern void NGCALLF(digradgc,DIGRADGC)(int *,int *,int *,int *,double
*,int *,int *,double *,double *, int *,int *,double *,int *,double *,int
*,int *);

extern void NGCALLF(dislapec,DISLAPEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *,double *,int *,
double *,int *,double *,int *);

extern void NGCALLF(dislapgc,DISLAPGC)(int *,int *,int *,int
*,double *,double *, int *,int *,double *,double *,int *,int *,double
*,int *, double *,int *,double *,int *);

extern void NGCALLF(divlapec,DIVLAPEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(divlapgc,DIVLAPGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(divrtec,DIVRTEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *, double *,int
*,double *,int *,double *,int *);

extern void NGCALLF(divrtgc,DIVRTGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,int *,int *, double *,int
*,double *,int *,double *,int *);

extern void NGCALLF(dmatgeo,DMATGEO)(int *,int *,double *,double *);

extern void NGCALLF(dmatgeov,DMATGEOV)(int *,int *,double *,double *,double *);

extern void NGCALLF(dsfvpec,DSFVPEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(dsfvpgc,DSFVPGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(dshaec,DSHAEC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *,int *,int *,double *,int *,double *,int *,int *);

extern void NGCALLF(dshaeci,DSHAECI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dshagc,DSHAGC)(int *,int *,int *,int *,double *,int
*,int *, double *,double *,int *,int *,double *,int *,double *,int *,int
*);
extern void NGCALLF(dshagci,DSHAGCI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dshsec,DSHSEC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *, int *,int *,double *,int *,double *,int *,int
*);

extern void NGCALLF(dshseci,DSHSECI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dshsgci,DSHSGCI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dshsgc,DSHSGC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *,int *,int *,double *,int *,double *,int *,int
*);

extern void NGCALLF(dslapec,DSLAPEC)(int *,int *,int *,int
*,double *,int *,int *,double *,double *, int *,int *,double *,int
*,double *,int *,int *);

extern void NGCALLF(dslapgc,DSLAPGC)(int *,int *,int *,int
*,double *,int *,int *,double *,double *, int *,int *,double *,int
*,double *,int *,int *);

extern void NGCALLF(dvhaec,DVHAEC)(int *,int *,int *,int *,double *,double
*, int *,int *,double *,double *,double *,double *,int *,int *,double *,int
*, double *,int *,int *);

extern void NGCALLF(dvhaeci,DVHAECI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dvhagc,DVHAGC)(int *,int *,int *,int *,double *,double
*, int *,int *,double *,double *,double *,double *,int *,int *,double *,int
*, double *,int *,int *);

extern void NGCALLF(dvhagci,DVHAGCI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dvhsec,DVHSEC)(int *,int *,int *,int *,double *,double
*, int *,int *,double *,double *,double *,double *,int *,int *, double
*,int *,double *,int *,int *);

extern void NGCALLF(dvhseci,DVHSECI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dvhsgc,DVHSGC)(int *,int *,int *,int *,double *,double
*, int *,int *,double *,double *,double *,double *,int *,int *, double
*,int *,double *,int *,int *);

extern void NGCALLF(dvhsgci,DVHSGCI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dvlapec,DVLAPEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int
*,double *,int *, double *,int *,int *);

extern void NGCALLF(dvlapgc,DVLAPGC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(dvrtec,DVRTEC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *, int *,int *,double *,int *,double *,int *,int *);

extern void NGCALLF(dvrtgc,DVRTGC)(int *,int *,int *,int *,double *,int
*,int *,double *,double *, int *,int *,double *,int *,double *,int *,int
*);
extern void NGCALLF(dvtsec,DVTSEC)(int *,int *,int *,int *,double *,double
*,int *,int *,double *,double *,double *,double *,int *,int *,double *,int *,
double *,int *,int *);

extern void NGCALLF(dvtseci,DVTSECI)(int *,int *,double *,int *,double
*,int *,int *);

extern void NGCALLF(dvtsgc,DVTSGC)(int *,int *,int *,int *,double *,double
*,int *,int *, double *,double *,double *,double *,int *,int *,double *,int
*, double *,int *,int *);

extern void NGCALLF(dvtsgci,DVTSGCI)(int *,int *,double *,int *,double
*,int *,int *);

NhlErrorTypes dv2uvf_W( void )
{
/*
 * Input array variables
 */
  void *dv;
  double *ddv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS], nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing;
/*
 * Output array variables
 */
  void *ud, *vd;
  double *dud, *dvd;
  float *rud, *rvd;
  int dsizes_ud[NCL_MAX_DIMENSIONS], dsizes_vd[NCL_MAX_DIMENSIONS];
  int ndims_ud, ndims_vd;
  NclBasicDataTypes type_ud, type_vd;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  double *work, *wshaec, *wvhsec, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (void*)NclGetArgValue(
           0,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           2);
/*
 * Get output arrays.
 */
  ud = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           NULL,
           NULL,
           &type_ud,
           1);
  vd = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           NULL,
           NULL,
           &type_vd,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be 2 or 3-dimensional.
 */
  if((type_ud != NCL_float && type_ud != NCL_double) ||
     (type_dv != NCL_float && type_dv != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
  if( ndims_ud != ndims_dv || ndims_vd != ndims_dv ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_dv; i++ ) {
    if( dsizes_ud[i] != dsizes_dv[i] || dsizes_vd[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_dv,ndims_dv,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce ddv.
 */
  ddv = coerce_input_double(dv,type_dv,total_size_in,has_missing_dv,
                            &missing_dv,&missing_ddv,NULL);
  if(ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dud and dvd are double.
 */
  dud = coerce_output_double(ud,type_ud,total_size_in);
  dvd = coerce_output_double(vd,type_vd,total_size_in);
  if(dud == NULL || dvd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(ddv,total_size_in,has_missing_dv,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&ddv[j],work);
    j += nlatnlon;
  }
  NclFree(work);

/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work   = (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,
                       a,b,&mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
/*
 * Free memory.
 */
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("dv2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/*
 * Allocate work arrays.
 */
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);
/*
 * Allocate work arrays.
 */
  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc( ldwork*sizeof(double),1);
  pertrb = (double*)calloc(    nt*sizeof(double),1);
  if( pertrb == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/* 
 * Reconstruct the divergent (irrotational) wind components.
 * Note the argument order idivec(...,vd,ud,...)
 */
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(didivec,DIDIVEC)(&nlat,&nlon,&isym,&nt,&dvd[0],&dud[0],
                         &idvw,&jdvw,a,b,&mdab,&ndab,wvhsec,&lvhsec,
                         work,&lwork,pertrb,&ker);
/*
 * Free memory.
 */
  NclFree(a);
  NclFree(b);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);
  NGCALLF(dchkerr,DCHKERR)("dv2uvf","vhseci,divec",&ier,&jer,&ker,&mer,6,12);
/*
 * Allocate space for work array.
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dud[j],&dvd[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dud[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvd[0],&scale,&ner);
/*
 * NclFree workspace array.
 */
  NclFree(work);
  if((void*)ddv != dv) NclFree(ddv);

  if(type_ud == NCL_float) rud = coerce_output_float(dud,ud,total_size_in,1);
  if(type_vd == NCL_float) rvd = coerce_output_float(dvd,vd,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes dv2uvg_W( void )
{
/*
 * Input array variables
 */
  void *dv;
  double *ddv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS], nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing;
/*
 * Output array variables
 */
  void *ud, *vd;
  double *dud, *dvd;
  float *rud, *rvd;
  int dsizes_ud[NCL_MAX_DIMENSIONS], dsizes_vd[NCL_MAX_DIMENSIONS];
  int ndims_ud, ndims_vd;
  NclBasicDataTypes type_ud, type_vd;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  double *work, *wshagc, *wvhsgc, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (void*)NclGetArgValue(
           0,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           2);
/*
 * Get output arrays.
 */
  ud = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           NULL,
           NULL,
           &type_ud,
           1);
  vd = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           NULL,
           NULL,
           &type_vd,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be 2 or 3-dimensional.
 */
  if((type_ud != NCL_float && type_ud != NCL_double) ||
     (type_dv != NCL_float && type_dv != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
  if( ndims_ud != ndims_dv || ndims_vd != ndims_dv ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_dv; i++ ) {
    if( dsizes_ud[i] != dsizes_dv[i] || dsizes_vd[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_dv,ndims_dv,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce dv.
 */
  ddv = coerce_input_double(dv,type_dv,total_size_in,has_missing_dv,
                            &missing_dv,&missing_ddv,NULL);
  if(ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dud and dvd are double.
 */
  dud = coerce_output_double(ud,type_ud,total_size_in);
  dvd = coerce_output_double(vd,type_vd,total_size_in);
  if(dud == NULL || dvd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(ddv,total_size_in,has_missing_dv,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&ddv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork *= 10;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,
                       a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
/*
 * Free memory.
 */
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("dv2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/*
 * Allocate work arrays.
 */
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;
/*
 * Allocate work arrays.
 */
  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertrb = (double*)calloc(    nt*sizeof(double),1);
  if( pertrb == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/* 
 * reconstruct the divergent (irrotational) wind components.
 * note the argument order idivgc(...,vd,ud,...)
 */
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(didivgc,DIDIVGC)(&nlat,&nlon,&isym,&nt,&dvd[0],&dud[0],
                         &idvw,&jdvw,a,b,&mdab,&ndab,wvhsgc,&lvhsgc,
                         work,&lwork,pertrb,&ker);
/*
 * Free memory.
 */
  NclFree(a);
  NclFree(b);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);

  NGCALLF(dchkerr,DCHKERR)("dv2uvg","vhsgci,divgc",&ier,&jer,&ker,&mer,6,12);
/*
 * Allocate space for work array.
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dud[j],&dvd[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dud[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvd[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)ddv != dv) NclFree(ddv);

  if(type_ud == NCL_float) rud = coerce_output_float(dud,ud,total_size_in,1);
  if(type_vd == NCL_float) rvd = coerce_output_float(dvd,vd,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes gradsf_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS], nt, nlat, nlon, nlatnlon;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
/*
 * Output array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  float *rgzx, *rgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclBasicDataTypes type_gzx, type_gzy;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  double *work, *wshaec, *wvhsec, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * Get output arrays.
 */
  gzx = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           NULL,
           NULL,
           &type_gzx,
           1);
  gzy = (void*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           NULL,
           NULL,
           &type_gzy,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_z || ndims_gzy != ndims_z ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_gzx[i] != dsizes_z[i] || dsizes_gzy[i] != dsizes_z[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_gzx != NCL_float && type_gzx != NCL_double) ||
     (type_gzy != NCL_float && type_gzy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,
                           &missing_z,&missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure gzx and gzy are double.
 */
  dgzx = coerce_output_double(gzx,type_gzx,total_size_in);
  dgzy = coerce_output_double(gzy,type_gzy,total_size_in);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,
                       a,b,&mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("gradsf","shaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the gradient.
 * note the argument order (...,gzy,gzx,...)
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(2*l1*nt+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  lwork *= 10;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);
  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(dgradec,DGRADEC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                         &idvw,&jdvw,a,b,&mdab,&ndab,wvhsec,&lvhsec,
                         work,&lwork,&ker);

  NclFree(a);
  NclFree(b);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("gradsf","vhseci+gradec",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dgzx[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dgzy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);

  if(type_gzx == NCL_float) rgzx = coerce_output_float(dgzx,gzx,total_size_in,1);
  if(type_gzy == NCL_float) rgzy = coerce_output_float(dgzy,gzy,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes gradsg_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS], nt, nlat, nlon, nlatnlon;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
/*
 * Output array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  float *rgzx, *rgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclBasicDataTypes type_gzx, type_gzy;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  double *work, *wshagc, *wvhsgc, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * Get output arrays.
 */
  gzx = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           NULL,
           NULL,
           &type_gzx,
           1);
  gzy = (void*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           NULL,
           NULL,
           &type_gzy,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_z || ndims_gzy != ndims_z ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_gzx[i] != dsizes_z[i] || dsizes_gzy[i] != dsizes_z[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_gzx != NCL_float && type_gzx != NCL_double) ||
     (type_gzy != NCL_float && type_gzy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,&missing_z,
                           &missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure gzx and gzy are double.
 */
  dgzx = coerce_output_double(gzx,type_gzx,total_size_in);
  dgzy = coerce_output_double(gzy,type_gzy,total_size_in);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,
                       a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("gradsg","shagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the gradient.
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(2*l1*nt+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  lwork *= 10;
  lvhsgc *= 10;
  ldwork = 2*nlat*(nlat+1)+1;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(dgradgc,DGRADGC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                         &idvw,&jdvw,a,b,&mdab,&ndab,wvhsgc,&lvhsgc,
                         work,&lwork,&ker);

  NclFree(a);
  NclFree(b);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("gradsg","vhsgci+gradgc",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dgzx[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dgzy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);

  if(type_gzx == NCL_float) rgzx = coerce_output_float(dgzx,gzx,total_size_in,1);
  if(type_gzy == NCL_float) rgzy = coerce_output_float(dgzy,gzy,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}

NhlErrorTypes igradsf_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  double missing;
  int has_missing_gzx, has_missing_gzy, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *dz;
  float *rz;
  int dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclBasicDataTypes type_z;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (void*)NclGetArgValue(
           0,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           2);
  gzy = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           2);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if((type_z != NCL_float && type_z != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The output array must be float or double");
    return(NhlFATAL);
  }
  if( ndims_z != ndims_gzx  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_z[i] != dsizes_gzx[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_gzx,ndims_gzx,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce gzx and gzy
 */
  dgzx = coerce_input_double(gzx,type_gzx,total_size_in,has_missing_gzx,
                             &missing_gzx,&missing_dgzx,NULL);
  dgzy = coerce_input_double(gzy,type_gzy,total_size_in,has_missing_gzy,
                             &missing_gzy,&missing_dgzy,NULL);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure z is double.
 */
  dz = coerce_output_double(z,type_z,total_size_in);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(dgzx,total_size_in,has_missing_gzx,
                                   missing_dgzx.doubleval);
  found_missing = contains_missing(dgzy,total_size_in,has_missing_gzy,
                                   missing_dgzy.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  lwork  *= 10;
  lvhaec *= 10;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec =  (double*)calloc(        lvhaec*sizeof(double),1);
  work   =  (double*)calloc(         lwork*sizeof(double),1);
  dwork  =  (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
                       work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("igradsf","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  lwork  *= 10;
  lshsec *= 10;
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(digradec,DIGRADEC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,br,bi,
                           &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  if (ker) {
    printf("shseci+igradec: lwork,lshsec= %i7 %i7\n",lwork,lshsec);
  }

  NclFree(br);
  NclFree(bi);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dz[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dgzx != gzx) NclFree(dgzx);
  if((void*)dgzy != gzy) NclFree(dgzy);

  if(type_z == NCL_float) rz = coerce_output_float(dz,z,total_size_in,1);

/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes igradsF_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  double missing;
  int has_missing_gzx, has_missing_gzy, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *z;
  float *rz;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (void*)NclGetArgValue(
           0,
           2,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           2);
  gzy = (void*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_gzx,ndims_gzx,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce gzx and gzy
 */
  dgzx = coerce_input_double(gzx,type_gzx,total_size_in,has_missing_gzx,
                             &missing_gzx,&missing_dgzx,NULL);
  dgzy = coerce_input_double(gzy,type_gzy,total_size_in,has_missing_gzy,
                             &missing_gzy,&missing_dgzy,NULL);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  z = (double*)calloc(total_size_in*sizeof(double),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dgzx,total_size_in,has_missing_gzx,
                                   missing_dgzx.doubleval);
  found_missing = contains_missing(dgzy,total_size_in,has_missing_gzy,
                                   missing_dgzy.doubleval);
  if(found_missing) {
/*
 * Return all missing values.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsF: The input arrays cannot contain any missing values");

    if(has_missing_gzx) missing = missing_dgzx.doubleval;
    else                missing = missing_dgzy.doubleval;
    if(type_gzx != NCL_double && type_gzy != NCL_double) {
/*
 * Return float missing values. 
 */
      rz = coerce_output_float_missing(z,total_size_in,missing);
      if( rz == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rz,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) z[i] = missing;
 
      return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  lwork  *= 10;
  lvhaec *= 10;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec =  (double*)calloc(        lvhaec*sizeof(double),1);
  work   =  (double*)calloc(         lwork*sizeof(double),1);
  dwork  =  (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
                       work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("igradsF","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  lwork  *= 10;
  lshsec *= 10;
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(digradec,DIGRADEC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
                           &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NGCALLF(dchkerr,DCHKERR)("igradsg","shseci+igradec",&ier,&jer,&ker,&mer,9,14);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dgzx != gzx) NclFree(dgzx);
  if((void*)dgzy != gzy) NclFree(dgzy);
/*
 * Return array.
 */
  if(type_gzx != NCL_double && type_gzy != NCL_double) {
    rz = (float*)NclMalloc(total_size_in*sizeof(float));
    if (rz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_in; i++ ) rz[i] = (float)z[i];
/*
 * Free double precision array.
 */
    NclFree(z);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rz,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_double,0));
  }
}



NhlErrorTypes igradsg_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  double missing;
  int has_missing_gzx, has_missing_gzy, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *dz;
  float *rz;
  int dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclBasicDataTypes type_z;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (void*)NclGetArgValue(
           0,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           2);
  gzy = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           2);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if((type_z != NCL_float && type_z != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The output array must be float or double");
    return(NhlFATAL);
  }
  if( ndims_z != ndims_gzx  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_z[i] != dsizes_gzx[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_gzx,ndims_gzx,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce gzx and gzy
 */
  dgzx = coerce_input_double(gzx,type_gzx,total_size_in,has_missing_gzx,
                             &missing_gzx,&missing_dgzx,NULL);
  dgzy = coerce_input_double(gzy,type_gzy,total_size_in,has_missing_gzy,
                             &missing_gzy,&missing_dgzy,NULL);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure z is double.
 */
  dz = coerce_output_double(z,type_z,total_size_in);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dgzx,total_size_in,has_missing_gzx,
                                   missing_dgzx.doubleval);
  found_missing = contains_missing(dgzy,total_size_in,has_missing_gzy,
                                   missing_dgzy.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                       &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhagc,&lvhagc,
                       work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("igradsg","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(digradgc,DIGRADGC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,br,bi,
                             &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(dchkerr,DCHKERR)("igradsg","shsgci+igradgc",&ier,&jer,&ker,&mer,9,14);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dz[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dgzx != gzx) NclFree(dgzx);
  if((void*)dgzy != gzy) NclFree(dgzy);

  if(type_z == NCL_float) rz = coerce_output_float(dz,z,total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes igradsG_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *dgzx, *dgzy;
  int dsizes_gzx[NCL_MAX_DIMENSIONS], dsizes_gzy[NCL_MAX_DIMENSIONS];
  int ndims_gzx, ndims_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  double missing;
  int has_missing_gzx, has_missing_gzy, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *z;
  float *rz;
/*
 * various
 */
  int total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  gzx = (void*)NclGetArgValue(
           0,
           2,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           2);
  gzy = (void*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_gzx; i++ ) {
    if( dsizes_gzx[i] != dsizes_gzy[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_gzx,ndims_gzx,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce gzx and gzy
 */
  dgzx = coerce_input_double(gzx,type_gzx,total_size_in,has_missing_gzx,
                             &missing_gzx,&missing_dgzx,NULL);
  dgzy = coerce_input_double(gzy,type_gzy,total_size_in,has_missing_gzy,
                             &missing_gzy,&missing_dgzy,NULL);
  if(dgzx == NULL || dgzy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  z = (double*)calloc(total_size_in*sizeof(double),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dgzx,total_size_in,has_missing_gzx,
                                   missing_dgzx.doubleval);
  found_missing = contains_missing(dgzy,total_size_in,has_missing_gzy,
                                   missing_dgzy.doubleval);
  if(found_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsG: The input arrays cannot contain any missing values");
/*
 * Return all missing values.
 */ 
    if(has_missing_gzx) missing = missing_dgzx.doubleval;
    else                missing = missing_dgzy.doubleval;

    if(type_gzx != NCL_double && type_gzy != NCL_double) {
/*
 * Return float missing values. 
 */
      rz = coerce_output_float_missing(z,total_size_in,missing);
      if( rz == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rz,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) z[i] = missing;
 
      return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dgzy[0],&dgzx[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhagc,&lvhagc,
                         work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("igradsG","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute the scalar function given the input vector
 */
  l3     = max(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l3+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(digradgc,DIGRADGC)(&nlat,&nlon,&isym,&nt,&z[0],&idvw,&jdvw,br,bi,
                             &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(dchkerr,DCHKERR)("igradsG","shsgci+igradgc",&ier,&jer,&ker,&mer,9,14);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dgzx[j],&dgzy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dgzx != gzx) NclFree(dgzx);
  if((void*)dgzy != gzy) NclFree(dgzy);
/*
 * Return array.
 */
  if(type_gzx != NCL_double && type_gzy != NCL_double) {
    rz = (float*)NclMalloc(total_size_in*sizeof(float));
    if (rz == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_in; i++ ) rz[i] = (float)z[i];
/*
 * Free double precision array.
 */
    NclFree(z);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rz,ndims_gzx,dsizes_gzx,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)z,ndims_gzx,dsizes_gzx,NULL,NCL_double,0));
  }
}



NhlErrorTypes ilapsf_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *dzlap, *dzlmbda, *zlmbda2;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  double missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *dz;
  float *rz;
  NclBasicDataTypes type_z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int total_size_in, total_size_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  double *work, *wshaec, *wshsec, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (void*)NclGetArgValue(
           0,
           3,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           &type_zlap,
           2);
  zlmbda = (void*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           2);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           1);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_zlap; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_z != NCL_float && type_z != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_zlmbda = 1;
  for(i = 0; i < ndims_zlmbda; i++)  total_size_zlmbda *= dsizes_zlmbda[i];
/*
 * Coerce zlap and zlmbda.
 */
  dzlap = coerce_input_double(zlap,type_zlap,total_size_in,has_missing_zlap,
                              &missing_zlap,&missing_dzlap,NULL);
  dzlmbda = coerce_input_double(zlmbda,type_zlmbda,total_size_zlmbda,
                                has_missing_zlmbda,
                                &missing_zlmbda,&missing_dzlmbda,NULL);
  if(dzlap == NULL || dzlmbda == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is a constant, and zlap has more
 * than 2 dimensions, then we need to allocate space for zlmbda.
 */
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
    zlmbda2 = dzlmbda;
  }
  else {
    if( ndims_zlmbda == 1 && dsizes_zlmbda[0] == 1 ) {
      zlmbda2 = (double*)calloc(total_size_zlmbda*sizeof(double),1);
      if( zlmbda2 == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for zlmbda array");
        return(NhlFATAL);
      }
      for( i = 0; i < total_size_zlmbda; i++ ) zlmbda2[i] = *dzlmbda;

      NclFree(dzlmbda);
    }
    else {
      for( i = 0; i < ndims_zlmbda; i++ ) {
        if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
          return(NhlFATAL);
        }
      }
      zlmbda2 = dzlmbda;
    }
  }
/*
 * Make sure z is double.
 */
  dz = coerce_output_double(z,type_z,total_size_in);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(zlmbda2,nt,has_missing_zlmbda,
                                   missing_dzlmbda.doubleval);
  if(found_missing) {
    missing = missing_dzlmbda.doubleval;
  }
  else {
    found_missing = contains_missing(dzlap,total_size_in,has_missing_zlap,
                                     missing_dzlap.doubleval);
    if(found_missing) missing = missing_dzlap.doubleval;
  }

  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dzlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;
  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapsf","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  l1    = min(nlat,(nlon+2)/2);
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertrb = (double*)calloc(     nt*sizeof(double),1);

  if( pertrb == NULL || wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dislapec,DISLAPEC)(&nlat,&nlon,&isym,&nt,&zlmbda2[0],&dz[0],
                             &idvw,&jdvw,a,b,&mdab,&ndab,wshsec,&lshsec,
                             work,&lwork,pertrb,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);
  NGCALLF(dchkerr,DCHKERR)("ilapsf","shseci+islapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dz[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dzlap != zlap) NclFree(dzlap);
  if((void*)zlmbda2 != zlmbda) NclFree(zlmbda2);

  if(type_z == NCL_float) rz = coerce_output_float(dz,z,total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapsF_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *dzlap, *dzlmbda, *zlmbda2;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  double missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *z;
  float *rz;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int total_size_in, total_size_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  double *work, *wshaec, *wshsec, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (void*)NclGetArgValue(
           0,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           &type_zlap,
           2);
  zlmbda = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_zlmbda = 1;
  for(i = 0; i < ndims_zlmbda; i++)  total_size_zlmbda *= dsizes_zlmbda[i];
/*
 * Coerce zlap and zlmbda.
 */
  dzlap = coerce_input_double(zlap,type_zlap,total_size_in,has_missing_zlap,
                              &missing_zlap,&missing_dzlap,NULL);
  dzlmbda = coerce_input_double(zlmbda,type_zlmbda,total_size_zlmbda,
                                has_missing_zlmbda,&missing_zlmbda,
                                &missing_dzlmbda,NULL);
  if(dzlap == NULL || dzlmbda == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is a constant, and zlap has more
 * than 2 dimensions, then we need to allocate space for zlmbda.
 */
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
    zlmbda2 = dzlmbda;
  }
  else {
    if( ndims_zlmbda == 1 && dsizes_zlmbda[0] == 1 ) {
      zlmbda2 = (double*)calloc(total_size_zlmbda*sizeof(double),1);
      if( zlmbda2 == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for zlmbda array");
        return(NhlFATAL);
      }
      for( i = 0; i < total_size_zlmbda; i++ ) zlmbda2[i] = *dzlmbda;

      NclFree(dzlmbda);
    }
    else {
      for( i = 0; i < ndims_zlmbda; i++ ) {
        if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
          return(NhlFATAL);
        }
      }
      zlmbda2 = dzlmbda;
    }
  }
/*
 * Allocate space for output array
 */
  z = (double*)calloc(total_size_in*sizeof(double),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(zlmbda2,nt,has_missing_zlmbda,
                                   missing_dzlmbda.doubleval);
  if(found_missing) {
    missing = missing_dzlmbda.doubleval;
  }
  else {
    found_missing = contains_missing(dzlap,total_size_in,has_missing_zlap,
                                     missing_dzlap.doubleval);
    if(found_missing) missing = missing_dzlap.doubleval;
  }

  if(found_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsF: The input arrays cannot contain any missing values");

    if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
/*
 * Return float missing values. 
 */
      rz = coerce_output_float_missing(z,total_size_in,missing);
      if( rz == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rz,ndims_zlap,dsizes_zlap,NULL,
                            NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) z[i] = missing;
      return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,
                            NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dzlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;
  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapsF","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  l1     = min(nlat,(nlon+2)/2);
  /*  lwork  = max(nlat+1,nlat*(2*nt*nlon+max(6*l2,nlon)+2*l3+1)); */
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertrb = (double*)calloc(     nt*sizeof(double),1);

  if( pertrb == NULL || wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dislapec,DISLAPEC)(&nlat,&nlon,&isym,&nt,&zlmbda2[0],&z[0],
                             &idvw,&jdvw,a,b,&mdab,&ndab,wshsec,&lshsec,
                             work,&lwork,pertrb,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);
  NGCALLF(dchkerr,DCHKERR)("ilapsF","shseci+islapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dzlap != zlap) NclFree(dzlap);
  if((void*)zlmbda2 != zlmbda) NclFree(zlmbda2);
/*
 * Return array.
 */
  if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
/*
 * Return float missing values. 
 */
    rz = (float*)calloc(total_size_in*sizeof(float),1);
    if( rz == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rz[i]  = (float)z[i];
    NclFree(z);   /* Free up the double array */
    return(NclReturnValue((void*)rz,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
  }
  else {
/*
 * Return double missing values. 
 */
    return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_double,0));
  }
}



NhlErrorTypes ilapsg_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *dzlap, *dzlmbda, *zlmbda2;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  double missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *dz;
  float *rz;
  NclBasicDataTypes type_z;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int total_size_in, total_size_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  double *work, *wshagc, *wshsgc, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (void*)NclGetArgValue(
           0,
           3,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           &type_zlap,
           2);
  zlmbda = (void*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           2);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           NULL,
           NULL,
           &type_z,
           1);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_zlap; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }  
  if(type_z != NCL_float && type_z != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_zlmbda = 1;
  for(i = 0; i < ndims_zlmbda; i++)  total_size_zlmbda *= dsizes_zlmbda[i];
/*
 * Coerce zlap and zlmbda.
 */
  dzlap = coerce_input_double(zlap,type_zlap,total_size_in,has_missing_zlap,
                              &missing_zlap,&missing_dzlap,NULL);
  dzlmbda = coerce_input_double(zlmbda,type_zlmbda,total_size_zlmbda,
                                has_missing_zlmbda,&missing_zlmbda,
                                &missing_dzlmbda,NULL);
  if(dzlap == NULL || dzlmbda == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is a constant, and zlap has more
 * than 2 dimensions, then we need to allocate space for zlmbda.
 */
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
    zlmbda2 = dzlmbda;
  }
  else {
    if( ndims_zlmbda == 1 && dsizes_zlmbda[0] == 1 ) {
      zlmbda2 = (double*)calloc(total_size_zlmbda*sizeof(double),1);
      if( zlmbda2 == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for zlmbda array");
        return(NhlFATAL);
      }
      for( i = 0; i < total_size_zlmbda; i++ ) zlmbda2[i] = *dzlmbda;

      NclFree(dzlmbda);
    }
    else {
      for( i = 0; i < ndims_zlmbda; i++ ) {
        if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
          return(NhlFATAL);
        }
      }
      zlmbda2 = dzlmbda;
    }
  }
/*
 * Make sure z is double.
 */
  dz = coerce_output_double(z,type_z,total_size_in);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(zlmbda2,nt,has_missing_zlmbda,
                                   missing_dzlmbda.doubleval);
  if(found_missing) {
    missing = missing_dzlmbda.doubleval;
  }
  else {
    found_missing = contains_missing(dzlap,total_size_in,has_missing_zlap,
                                     missing_dzlap.doubleval);
    if(found_missing) missing = missing_dzlap.doubleval;
  }

  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dzlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapsg","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;


  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  pertrb = (double*)calloc(     nt*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( pertrb == NULL || wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dislapgc,DISLAPGC)(&nlat,&nlon,&isym,&nt,&zlmbda2[0],&dz[0],
                             &idvw,&jdvw,a,b,&mdab,&ndab,wshsgc,&lshsgc,
                             work,&lwork,pertrb,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);
  NGCALLF(dchkerr,DCHKERR)("ilapsg","shsgci+islapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dz[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dzlap != zlap) NclFree(dzlap);
  if((void*)zlmbda2 != zlmbda) NclFree(zlmbda2);

  if(type_z == NCL_float) rz = coerce_output_float(dz,z,total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapsG_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *dzlap, *dzlmbda, *zlmbda2;
  int dsizes_zlap[NCL_MAX_DIMENSIONS], dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  int ndims_zlap, ndims_zlmbda;
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  double missing;
  int has_missing_zlap, has_missing_zlmbda, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *z;
  float *rz;
  int ndims_z, dsizes_z[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int total_size_in, total_size_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc; 
  double *work, *wshagc, *wshsgc, *pertrb, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  zlap = (void*)NclGetArgValue(
           0,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
           &type_zlap,
           2);
  zlmbda = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           2);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */

  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_zlmbda = 1;
  for(i = 0; i < ndims_zlmbda; i++)  total_size_zlmbda *= dsizes_zlmbda[i];
/*
 * Coerce zlap and zlmbda.
 */
  dzlap = coerce_input_double(zlap,type_zlap,total_size_in,has_missing_zlap,
                              &missing_zlap,&missing_dzlap,NULL);
  dzlmbda = coerce_input_double(zlmbda,type_zlmbda,total_size_zlmbda,
                                has_missing_zlmbda,&missing_zlmbda,
                                &missing_dzlmbda,NULL);
  if(dzlap == NULL || dzlmbda == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is a constant, and zlap has more
 * than 2 dimensions, then we need to allocate space for zlmbda.
 */
  if( ndims_zlap == 2 ) {
    if( ndims_zlmbda != 1 || dsizes_zlmbda[0] != 1 ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: If the first input array is two dimensional, then the second argument must be a constant");
      return(NhlFATAL);
    }
    zlmbda2 = dzlmbda;
  }
  else {
    if( ndims_zlmbda == 1 && dsizes_zlmbda[0] == 1 ) {
      zlmbda2 = (double*)calloc(total_size_zlmbda*sizeof(double),1);
      if( zlmbda2 == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for zlmbda array");
        return(NhlFATAL);
      }
      for( i = 0; i < total_size_zlmbda; i++ ) zlmbda2[i] = *dzlmbda;

      NclFree(dzlmbda);
    }
    else {
      for( i = 0; i < ndims_zlmbda; i++ ) {
        if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
          return(NhlFATAL);
        }
      }
      zlmbda2 = dzlmbda;
    }
  }
/*
 * Allocate space for output array
 */
  z = (double*)calloc(total_size_in*sizeof(double),1);
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(zlmbda2,nt,has_missing_zlmbda,
                                   missing_dzlmbda.doubleval);
  if(found_missing) {
    missing = missing_dzlmbda.doubleval;
  }
  else {
    found_missing = contains_missing(dzlap,total_size_in,has_missing_zlap,
                                     missing_dzlap.doubleval);
    if(found_missing) missing = missing_dzlap.doubleval;
  }

  if(found_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsG: The input arrays cannot contain any missing values");

    if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
/*
 * Return float missing values. 
 */
      rz = coerce_output_float_missing(z,total_size_in,missing);
      if( rz == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rz,ndims_zlap,dsizes_zlap,NULL,
                            NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) z[i] = missing;
      return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,
                            NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dzlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapsG","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * invert the laplacian
 */ 
  lwork  = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;


  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  pertrb = (double*)calloc(     nt*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( pertrb == NULL || wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dislapgc,DISLAPGC)(&nlat,&nlon,&isym,&nt,&zlmbda2[0],&z[0],
                             &idvw,&jdvw,a,b,&mdab,&ndab,wshsgc,&lshsgc,
                             work,&lwork,pertrb,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NclFree(pertrb);
  NGCALLF(dchkerr,DCHKERR)("ilapsG","shsgci+islapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&z[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&z[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dzlap != zlap) NclFree(dzlap);
  if((void*)zlmbda2 != zlmbda) NclFree(zlmbda2);
/*
 * Return array.
 */
  if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
    rz = (float*)calloc(total_size_in*sizeof(float),1);
    if( rz == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rz[i]  = (float)z[i];
    NclFree(z);   /* Free up the double array */
    return(NclReturnValue((void*)rz,ndims_zlap,dsizes_zlap,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)z,ndims_zlap,dsizes_zlap,NULL,NCL_double,0));
  }
}


NhlErrorTypes ilapvf_W( void )
{
/*
 * Input array variables
 */
  void *ulap, *vlap;
  double *dulap, *dvlap;
  int ndims_ulap, dsizes_ulap[NCL_MAX_DIMENSIONS];
  int ndims_vlap, dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap, missing_dulap, missing_dvlap;
  NclBasicDataTypes type_ulap, type_vlap;
  double missing;
  int has_missing_ulap, has_missing_vlap, found_missing;
/*
 * Output array variables
 */
  void *u, *v;
  double *du, *dv;
  float *ru, *rv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
  int ndims_u, ndims_v;
/*
 * various
 */
  int total_size_in, nt, nlat, nlon, nlatnlon;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lvhsec;
  double *work, *wvhaec, *wvhsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ulap = (void*)NclGetArgValue(
           0,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           &type_ulap,
           2);
  vlap = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           2);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_ulap[i] != dsizes_vlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_u[i] != dsizes_ulap[i] || dsizes_v[i] != dsizes_ulap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_ulap,ndims_ulap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce ulap and vlap.
 */
  dulap = coerce_input_double(ulap,type_ulap,total_size_in,has_missing_ulap,
                              &missing_ulap,&missing_dulap,NULL);
  dvlap = coerce_input_double(vlap,type_vlap,total_size_in,has_missing_vlap,
                              &missing_vlap,&missing_dvlap,NULL);
  if( dulap == NULL || dvlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double.
 */
  du = coerce_output_double(u,type_u,total_size_in);
  dv = coerce_output_double(v,type_v,total_size_in);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(dulap,total_size_in,has_missing_ulap,
                                   missing_dulap.doubleval);
  found_missing = contains_missing(dvlap,total_size_in,has_missing_vlap,
                                   missing_dvlap.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dvlap[0],&dulap[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhaec,&lvhaec,
                         work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  l1     = min(nlat,nlon/2);
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*nt*l1+1);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(divlapec,DIVLAPEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                             &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                             wvhsec,&lvhsec,work,&lwork,&ker);

  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("ilapvf","vhseci,ivlapec",&ier,&jer,&ker,&mer,
                           8,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&du[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dulap != ulap) NclFree(dulap);
  if((void*)dvlap != vlap) NclFree(dvlap);

  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_in,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes ilapvg_W( void )
{
/*
 * Input array variables
 */
  void *ulap, *vlap;
  double *dulap, *dvlap;
  int ndims_ulap, dsizes_ulap[NCL_MAX_DIMENSIONS];
  int ndims_vlap, dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap, missing_dulap, missing_dvlap;
  NclBasicDataTypes type_ulap, type_vlap;
  double missing;
  int has_missing_ulap, has_missing_vlap, found_missing;
/*
 * Output array variables
 */
  void *u, *v;
  double *du, *dv;
  float *ru, *rv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
  int ndims_u, ndims_v;
/*
 * various
 */
  int total_size_in, nt, nlat, nlon, nlatnlon;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsgc;
  double *work, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ulap = (void*)NclGetArgValue(
           0,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           &type_ulap,
           2);
  vlap = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           2);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_ulap[i] != dsizes_vlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_ulap; i++ ) {
    if( dsizes_u[i] != dsizes_ulap[i] || dsizes_v[i] != dsizes_ulap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_ulap,ndims_ulap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce ulap and vlap.
 */
  dulap = coerce_input_double(ulap,type_ulap,total_size_in,has_missing_ulap,
                              &missing_ulap,&missing_dulap,NULL);
  dvlap = coerce_input_double(vlap,type_vlap,total_size_in,has_missing_vlap,
                              &missing_vlap,&missing_dvlap,NULL);
  if( dulap == NULL || dvlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double.
 */
  du = coerce_output_double(u,type_u,total_size_in);
  dv = coerce_output_double(v,type_v,total_size_in);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dulap,total_size_in,has_missing_ulap,
                                   missing_dulap.doubleval);
  found_missing = contains_missing(dvlap,total_size_in,has_missing_vlap,
                                   missing_dvlap.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dvlap[0],&dulap[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("ilapvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  l1     = min(nlat,nlon/2);
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*nt*l1+1);
  lvhsgc = l1*l2*(2*nlat-l1+1)+nlon+15;

  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(divlapgc,DIVLAPGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                             &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                             wvhsgc,&lvhsgc,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("ilapvg","vhsgci,ivlapgc",&ier,&jer,&ker,&mer,
                           8,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(6.37122e+6,2.);         /* radius of earth**2 */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&du[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dulap != ulap) NclFree(dulap);
  if((void*)dvlap != vlap) NclFree(dvlap);

  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_in,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsf_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *dzlap;
  float *rzlap;
  NclBasicDataTypes type_zlap;
  int ndims_zlap, dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  double *work, *wshaec, *wshsec, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           2,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * Get output array.
 */
  zlap = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           NULL,
           NULL,
           &type_zlap,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_zlap != NCL_float && type_zlap != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,
                           &missing_z,&missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure zlap is double.
 */
  dzlap = coerce_output_double(zlap,type_zlap,total_size_in);
  if(dzlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsf","shaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dslapec,DSLAPEC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                           &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsf","vhseci+slapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
 
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dzlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);

  if(type_zlap == NCL_float) rzlap = coerce_output_float(dzlap,zlap,
                                                         total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsF_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *zlap;
  float *rzlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lshsec;
  double *work, *wshaec, *wshsec, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           1,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array
 */
  zlap = (double*)calloc(total_size_in*sizeof(double),1);
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,
                           &missing_z,&missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsF: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_z != NCL_double) {
/*
 * Return float missing values. 
 */
      rzlap = coerce_output_float_missing(zlap,total_size_in,
                                          missing_dz.doubleval);
      if( rzlap == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rzlap,ndims_z,dsizes_z,NULL,
                            NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) zlap[i] = missing_dz.doubleval;
      return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,
                            NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nlon*nt+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsF","shaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  l1     = min(nlat,(nlon+1)/2);
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dslapec,DSLAPEC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
                           &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsF","vhseci+slapec",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
 
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);
/*
 * Return array.
 */
  if(type_z != NCL_double) {
    rzlap = (float*)calloc(total_size_in*sizeof(float),1);
    if( rzlap == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rzlap[i]  = (float)zlap[i];
    NclFree(zlap);   /* Free up the double array */
    return(NclReturnValue((void*)rzlap,ndims_z,dsizes_z,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_double,0));
  }
}


NhlErrorTypes lapsg_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *dzlap;
  float *rzlap;
  NclBasicDataTypes type_zlap;
  int ndims_zlap, dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  double *work, *wshagc, *wshsgc, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           2,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * Get output array.
 */
  zlap = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           NULL,
           NULL,
           &type_zlap,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_z != ndims_zlap  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_z; i++ ) {
    if( dsizes_z[i] != dsizes_zlap[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_zlap != NCL_float && type_zlap != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,
                           &missing_z,&missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure zlap is double.
 */
  dzlap = coerce_output_double(zlap,type_zlap,total_size_in);
  if(dzlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsg","shagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dslapgc,DSLAPGC)(&nlat,&nlon,&isym,&nt,&dzlap[0],&idvw,&jdvw,a,b,
                           &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsg","vhsgci+slapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dzlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dzlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);

  if(type_zlap == NCL_float) rzlap = coerce_output_float(dzlap,zlap,
                                                         total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapsG_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *dz;
  int dsizes_z[NCL_MAX_DIMENSIONS], ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing;
  int nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  double *zlap;
  float *rzlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lshsgc;
  double *work, *wshagc, *wshsgc, *a, *b, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  z = (void*)NclGetArgValue(
           0,
           1,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_z < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_z,ndims_z,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array
 */
  zlap = (double*)calloc(total_size_in*sizeof(double),1);
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce z.
 */
  dz = coerce_input_double(z,type_z,total_size_in,has_missing_z,
                           &missing_z,&missing_dz,NULL);
  if(dz == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dz,total_size_in,has_missing_z,
                                   missing_dz.doubleval);
  if(found_missing) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsG: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_z != NCL_double) {
/*
 * Return float missing values. 
 */
      rzlap = coerce_output_float_missing(zlap,total_size_in,
                                          missing_dz.doubleval);
      if( rzlap == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rzlap,ndims_z,dsizes_z,NULL,
                            NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) zlap[i] = missing_dz.doubleval;
      return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,
                            NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dz[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dz[0],&idvw,&jdvw,a,b,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsG","shagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the laplacian
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work =   (double*)calloc( lwork*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dslapgc,DSLAPGC)(&nlat,&nlon,&isym,&nt,&zlap[0],&idvw,&jdvw,a,b,
                           &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NclFree(a);
  NclFree(b);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapsG","vhsgci+slapgc",&ier,&jer,&ker,&mer,7,13);
/*
 * transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dz[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&zlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&zlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dz != z) NclFree(dz);
/*
 * Return array.
 */
  if(type_z != NCL_double) {
    rzlap = (float*)calloc(total_size_in*sizeof(float),1);
    if( rzlap == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rzlap[i]  = (float)zlap[i];
    NclFree(zlap);   /* Free up the double array */
    return(NclReturnValue((void*)rzlap,ndims_z,dsizes_z,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)zlap,ndims_z,dsizes_z,NULL,NCL_double,0));
  }
}

NhlErrorTypes lapvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void *ulap, *vlap;
  double *dulap, *dvlap;
  float *rulap, *rvlap;
  int dsizes_ulap[NCL_MAX_DIMENSIONS], dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
  NclBasicDataTypes type_ulap, type_vlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int nt, nlat, nlon, nlatnlon, total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork,lvhaec, lvhsec;
  double *work, *wvhaec, *wvhsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (double*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (double*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  ulap = (double*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           NULL,
           NULL,
           &type_ulap,
           1);
  vlap = (double*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           NULL,
           NULL,
           &type_vlap,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input arrays must be at least 2-dimensional and have the same dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_ulap[i] != dsizes_u[i] || dsizes_vlap[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_ulap != NCL_float && type_ulap != NCL_double) ||
     (type_vlap != NCL_float && type_vlap != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure ulap and vlap are double.
 */
  dulap = coerce_output_double(ulap,type_ulap,total_size_in);
  dvlap = coerce_output_double(vlap,type_vlap,total_size_in);
  if(dulap == NULL || dvlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapvf","vhaec",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  l1     = min(nlat,(nlon+1)/2);
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+1)+4*(l1*nlat*nt);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(dvlapec,DVLAPEC)(&nlat,&nlon,&isym,&nt,&dvlap[0],&dulap[0],
                           &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,wvhsec,&lvhsec,
                           work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("lapvf","vhseci,vlapec",&ier,&jer,&ker,&mer,7,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
   scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
   NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dulap[0],&scale,&ner);
   NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_ulap == NCL_float) rulap = coerce_output_float(dulap,ulap,
                                                         total_size_in,1);
  if(type_vlap == NCL_float) rvlap = coerce_output_float(dvlap,vlap,
                                                         total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lapvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  int has_missing_u, has_missing_v, found_missing;
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void *ulap, *vlap;
  double *dulap, *dvlap;
  float *rulap, *rvlap;
  int dsizes_ulap[NCL_MAX_DIMENSIONS], dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
  NclBasicDataTypes type_ulap, type_vlap;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int nt, nlat, nlon, nlatnlon, total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsgc;
  double *work, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (double*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (double*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  ulap = (double*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           NULL,
           NULL,
           &type_ulap,
           1);
  vlap = (double*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           NULL,
           NULL,
           &type_vlap,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input arrays must be at least 2-dimensional and have the same dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_ulap || ndims_v != ndims_ulap ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_ulap[i] != dsizes_u[i] || dsizes_vlap[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_ulap != NCL_float && type_ulap != NCL_double) ||
     (type_vlap != NCL_float && type_vlap != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure ulap and vlap are double.
 */
  dulap = coerce_output_double(ulap,type_ulap,total_size_in);
  dvlap = coerce_output_double(vlap,type_vlap,total_size_in);
  if(dulap == NULL || dvlap == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);

  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lapvg","vhagc",&ier,&jer,&ker,&mer,7,5);
/* 
 * compute the vector laplacian using the vector spherical harmonic 
 */ 
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon)+1)+4*(l1*nlat*nt);
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(dvlapgc,DVLAPGC)(&nlat,&nlon,&isym,&nt,&dvlap[0],&dulap[0],
                           &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                           wvhsgc,&lvhsgc,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("lapvg","vhsgci,vlapgc",&ier,&jer,&ker,&mer,7,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dulap[j],&dvlap[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = powf(1./6.37122e+6,2.);       /* (1/(radius of earth))**2 */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dulap[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvlap[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_ulap == NCL_float) rulap = coerce_output_float(dulap,ulap,
                                                         total_size_in,1);
  if(type_vlap == NCL_float) rvlap = coerce_output_float(dvlap,vlap,
                                                         total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}



NhlErrorTypes uv2sfvpf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *sf, *vp;
  double *dsf, *dvp;
  float *rsf, *rvp;
  NclBasicDataTypes type_sf, type_vp;
  int dsizes_sf[NCL_MAX_DIMENSIONS], dsizes_vp[NCL_MAX_DIMENSIONS];
  int ndims_sf, ndims_vp;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int nt, nlat, nlon, nlatnlon, total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  sf = (void*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           NULL,
           NULL,
           &type_sf,
           1);
  vp = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           NULL,
           NULL,
           &type_vp,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must have the same dimensions as the input arrays.
 */
  if( ndims_sf != ndims_u || ndims_vp != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_u[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_sf != NCL_float && type_sf != NCL_double) ||
     (type_vp != NCL_float && type_vp != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure sf and vp are double.
 */
  dsf = coerce_output_double(sf,type_sf,total_size_in);
  dvp = coerce_output_double(vp,type_vp,total_size_in);
  if(dsf == NULL || dvp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2sfvpf","vhaec",&ier,&jer,&ker,&mer,10,5);

  l1     = min(nlat,(nlon+2)/2);
  lwork  = max(nlat+1,nlat*((nt*nlon+max(3*l2,nlon))+2*l1*nt+1));
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dsfvpec,DSFVPEC)(&nlat,&nlon,&isym,&nt,&dsf[0],&dvp[0],
                           &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                           wshsec,&lshsec,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("uv2sfvpf","sfvpec+shseci",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dsf[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvp[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;        /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dsf[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvp[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_sf == NCL_float) rsf = coerce_output_float(dsf,sf,total_size_in,1);
  if(type_vp == NCL_float) rvp = coerce_output_float(dvp,vp,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2sfvpg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *sf, *vp;
  double *dsf, *dvp;
  float *rsf, *rvp;
  NclBasicDataTypes type_sf, type_vp;
  int dsizes_sf[NCL_MAX_DIMENSIONS], dsizes_vp[NCL_MAX_DIMENSIONS];
  int ndims_sf, ndims_vp;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int total_size_in, nt, nlat, nlon, nlatnlon;
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  sf = (void*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           NULL,
           NULL,
           &type_sf,
           1);
  vp = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           NULL,
           NULL,
           &type_vp,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must have the same dimensions as the input arrays.
 */
  if( ndims_sf != ndims_u || ndims_vp != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_u[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_sf != NCL_float && type_sf != NCL_double) ||
     (type_vp != NCL_float && type_vp != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure sf and vp are double.
 */
  dsf = coerce_output_double(sf,type_sf,total_size_in);
  dvp = coerce_output_double(vp,type_vp,total_size_in);
  if(dsf == NULL || dvp == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2sfvpg","vhagc",&ier,&jer,&ker,&mer,10,5);

  l1     = min(nlat,(nlon+2)/2);
  lwork  = max(4*nlat*(nlat+2)+2,nlat*((nt*nlon+max(3*l2,nlon))+2*l1*nt+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dsfvpgc,DSFVPGC)(&nlat,&nlon,&isym,&nt,&dsf[0],&dvp[0],
                           &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                           wshsgc,&lshsgc,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("uv2sfvpg","sfvpgc+shsgci",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dsf[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvp[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;        /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dsf[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvp[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_sf == NCL_float) rsf = coerce_output_float(dsf,sf,total_size_in,1);
  if(type_vp == NCL_float) rvp = coerce_output_float(dvp,vp,total_size_in,1);

/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lderuvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *uy, *vy;
  double *duy, *dvy;
  float *ruy, *rvy;
  NclBasicDataTypes type_uy, type_vy;
  int dsizes_uy[NCL_MAX_DIMENSIONS], dsizes_vy[NCL_MAX_DIMENSIONS];
  int ndims_uy, ndims_vy;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lwvts;
  double *work, *wvhaec, *wvts, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  uy = (void*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           NULL,
           NULL,
           &type_uy,
           1);
  vy = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           NULL,
           NULL,
           &type_vy,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_uy != ndims_u || ndims_vy != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_uy[i] != dsizes_u[i] || dsizes_vy[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_uy != NCL_float && type_uy != NCL_double) ||
     (type_vy != NCL_float && type_vy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure uy and vy are double.
 */
  duy = coerce_output_double(uy,type_uy,total_size_in);
  dvy = coerce_output_double(vy,type_vy,total_size_in);
  if(duy == NULL || dvy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhaec(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));*/
  lwork = nlat*(2*nt*nlon+max(6*l2,nlon));
  ldwork = 2*(nlat+2);
  lwvts  = 4*nlat*l2 +3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon +15;
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  wvts =   (double*)calloc(         lwvts*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvts == NULL || wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lderuvf","vhaec",&ier,&jer,&ker,&mer,9,5);
/*
 * compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 

  lwork = nlat*(2*nt*nlon+max(6*l2,nlon));
  work  = (double*)calloc(lwork*sizeof(double),1);
  ldwork = 2*(nlat+1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvtseci,DVTSECI)(&nlat,&nlon,wvts,&lwvts,dwork,&ldwork,&jer);
  NGCALLF(dvtsec,DVTSEC)(&nlat,&nlon,&ityp,&nt,&dvy[0],&duy[0],&idvw,&jdvw,
                         br,bi,cr,ci,&mdab,&ndab,wvts,&lwvts,
                         work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvts);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("lderuvf","vtsec",&ier,&jer,&ker,&mer,9,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&duy[j],&dvy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&duy[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_uy == NCL_float) ruy = coerce_output_float(duy,uy,total_size_in,1);
  if(type_vy == NCL_float) rvy = coerce_output_float(dvy,vy,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes lderuvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *uy, *vy;
  double *duy, *dvy;
  float *ruy, *rvy;
  NclBasicDataTypes type_uy, type_vy;
  int dsizes_uy[NCL_MAX_DIMENSIONS], dsizes_vy[NCL_MAX_DIMENSIONS];
  int ndims_uy, ndims_vy;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lwvts;
  double *work, *wvhagc, *wvts, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  uy = (void*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           NULL,
           NULL,
           &type_uy,
           1);
  vy = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           NULL,
           NULL,
           &type_vy,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_uy != ndims_u || ndims_vy != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_uy[i] != dsizes_u[i] || dsizes_vy[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_uy != NCL_float && type_uy != NCL_double) ||
     (type_vy != NCL_float && type_vy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure uy and vy are double.
 */
  duy = coerce_output_double(uy,type_uy,total_size_in);
  dvy = coerce_output_double(vy,type_vy,total_size_in);
  if(duy == NULL || dvy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space *
 * Note the order "vhagc(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  ldwork = 2*nlat*(nlat+1)+1;
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+l2+15;

  br =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("lderuvg","vhagc",&ier,&jer,&ker,&mer,9,5);
/*
 * compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 

  lwvts  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lwork  = nlat*(2*nt*nlon+max(6*l2,nlon));
  ldwork = nlat*(nlat+4);

  wvts   =  (double*)calloc( lwvts*sizeof(double),1);
  work   =  (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvts == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvtsgci,DVTSGCI)(&nlat,&nlon,wvts,&lwvts,dwork,&ldwork,&jer);
  NGCALLF(dvtsgc,DVTSGC)(&nlat,&nlon,&ityp,&nt,&dvy[0],&duy[0],&idvw,&jdvw,
                         br,bi,cr,ci,&mdab,&ndab,wvts,&lwvts,
                         work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvts);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("lderuvg","vtsec",&ier,&jer,&ker,&mer,9,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&duy[j],&dvy[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&duy[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvy[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_uy == NCL_float) ruy = coerce_output_float(duy,uy,total_size_in,1);
  if(type_vy == NCL_float) rvy = coerce_output_float(dvy,vy,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *dvo;
  double *ddv;
  float *rdv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_dv;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output array.
 */
  dvo = (void*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_dv != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_dv != NCL_float && type_dv != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dv is double.
 */
  ddv = coerce_output_double(dvo,type_dv,total_size_in);
  if(ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2dvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lwork  = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(ddivec,DDIVEC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2dvf","shseci+divec",&ier,&jer,&ker,&mer,8,12);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_dv == NCL_float) rdv = coerce_output_float(ddv,dvo,total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
  double missing;
/*
 * Output array variables
 */
  double *ddv;
  float *rdv;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array.
 */
  ddv = (double*)calloc(total_size_in*sizeof(double),1);
  if( ddv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    if(has_missing_u) missing = missing_du.doubleval;
    else              missing = missing_dv.doubleval;
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvF: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_u != NCL_double && type_v != NCL_double) {
/*
 * Return float missing values. 
 */
      rdv = coerce_output_float_missing(ddv,total_size_in,missing);
      if( rdv == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rdv,ndims_u,dsizes_u,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) ddv[i] = missing;
      return(NclReturnValue((void*)ddv,ndims_u,dsizes_u,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2dvF","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lwork  = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(ddivec,DDIVEC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2dvF","shseci+divec",&ier,&jer,&ker,&mer,8,12);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);
/*
 * Return array.
 */
  if(type_u != NCL_double && type_v != NCL_double){
/*
 * Coerce values back to float if necessary.
 */
    rdv = (float*)calloc(total_size_in*sizeof(float),1);
    if( rdv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rdv[i] = (float)ddv[i];
    NclFree(ddv);   /* Free up the double array */
    return(NclReturnValue((void*)rdv,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)ddv,ndims_u,dsizes_u,NULL,NCL_double,0));
  }
}


NhlErrorTypes uv2dvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *dvo;
  double *ddv;
  float *rdv;
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_dv;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output array.
 */
  dvo = (void*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_dv != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_dv != NCL_float && type_dv != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dv is double.
 */
  ddv = coerce_output_double(dvo,type_dv,total_size_in);
  if(ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2dvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(ddivgc,DDIVGC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2dvg","shsgci+divgc",&ier,&jer,&ker,&mer,8,12);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_dv == NCL_float) rdv = coerce_output_float(ddv,dvo,total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2dvG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  double *ddv;
  float *rdv;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array.
 */
  ddv = (double*)calloc(total_size_in*sizeof(double),1);
  if( ddv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    if(has_missing_u) missing = missing_du.doubleval;
    else              missing = missing_dv.doubleval;
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvG: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_u != NCL_double && type_v != NCL_double) {
/*
 * Return float missing values. 
 */
      rdv = coerce_output_float_missing(ddv,total_size_in,missing);
      if( rdv == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rdv,ndims_u,dsizes_u,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) ddv[i] = missing;
      return(NclReturnValue((void*)ddv,ndims_u,dsizes_u,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(cr);
  NclFree(ci);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2dvG","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(ddivgc,DDIVGC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2dvG","shsgci+divgc",&ier,&jer,&ker,&mer,8,12);

  NclFree(br);
  NclFree(bi);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);
/*
 * Return array.
 */
  if(type_u != NCL_double && type_v != NCL_double){
/*
 * Coerce values back to float if necessary.
 */
    rdv = (float*)calloc(total_size_in*sizeof(float),1);
    if( rdv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rdv[i] = (float)ddv[i];
    NclFree(ddv);   /* Free up the double array */
    return(NclReturnValue((void*)rdv,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)ddv,ndims_u,dsizes_u,NULL,NCL_double,0));
  }
}

NhlErrorTypes uv2vrf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *vort;
  double *dvort;
  float *rvort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_vort;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output array.
 */
  vort = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           NULL,
           NULL,
           &type_vort,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_vort != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vort[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_vort != NCL_float && type_vort != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure vort is double.
 */
  dvort = coerce_output_double(vort,type_vort,total_size_in);
  if(dvort == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrf","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  l1     = min(nlat,(nlon+2)/2);
  lwork = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dvrtec,DVRTEC)(&nlat,&nlon,&isym,&nt,&dvort[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2vrf","shseci+vrtec",&ier,&jer,&ker,&mer,8,12);

  NclFree(cr);
  NclFree(ci);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_vort == NCL_float) rvort = coerce_output_float(dvort,vort,
                                                         total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  double *dvort;
  float *rvort;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array.
 */
  dvort = (double*)calloc(total_size_in*sizeof(double),1);
  if( dvort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    if(has_missing_u) missing = missing_du.doubleval;
    else              missing = missing_dv.doubleval;
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrF: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_u != NCL_double && type_v != NCL_double) {
/*
 * Return float missing values. 
 */
      rvort = coerce_output_float_missing(dvort,total_size_in,missing);
      if( rvort == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rvort,ndims_u,dsizes_u,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) dvort[i] = missing;
      return(NclReturnValue((void*)dvort,ndims_u,dsizes_u,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrF","vhaec",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
  l1     = min(nlat,(nlon+2)/2);
  lwork = nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat+1;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wshsec = (double*)calloc(lshsec*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dvrtec,DVRTEC)(&nlat,&nlon,&isym,&nt,&dvort[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2vrF","shseci+vrtec",&ier,&jer,&ker,&mer,8,12);

  NclFree(cr);
  NclFree(ci);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
/*
 * Return array.
 */
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);
/*
 * Return array.
 */
  if(type_u != NCL_double && type_v != NCL_double){
/*
 * Coerce values back to float if necessary.
 */
    rvort = (float*)calloc(total_size_in*sizeof(float),1);
    if( rvort == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rvort[i] = (float)dvort[i];
    NclFree(dvort);   /* Free up the double array */
    return(NclReturnValue((void*)rvort,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dvort,ndims_u,dsizes_u,NULL,NCL_double,0));
  }
}


NhlErrorTypes uv2vrg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *vort;
  double *dvort;
  float *rvort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_vort;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           3,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output array.
 */
  vort = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           NULL,
           NULL,
           &type_vort,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output array must also be at least 2-dimensional.
 */
  if( ndims_vort != ndims_u  ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vort[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if(type_vort != NCL_float && type_vort != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure vort is double.
 */
  dvort = coerce_output_double(vort,type_vort,total_size_in);
  if(dvort == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  /*  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2)); */
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrg","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork = nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);

  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dvrtgc,DVRTGC)(&nlat,&nlon,&isym,&nt,&dvort[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2vrg","shsgci+vrtgc",&ier,&jer,&ker,&mer,8,12);

  NclFree(cr);
  NclFree(ci);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_vort == NCL_float) rvort = coerce_output_float(dvort,vort,
                                                         total_size_in,1);
/*
 * Return array.
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  double missing;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  double *dvort;
  float *rvort;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           2,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Allocate space for output array.
 */
  dvort = (double*)calloc(total_size_in*sizeof(double),1);
  if( dvort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    if(has_missing_u) missing = missing_du.doubleval;
    else              missing = missing_dv.doubleval;
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrG: The input arrays cannot contain any missing values");
/*
 * Coerce values back to float if necessary.
 */
    if(type_u != NCL_double && type_v != NCL_double) {
/*
 * Return float missing values. 
 */
      rvort = coerce_output_float_missing(dvort,total_size_in,missing);
      if( rvort == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for coercing output to single precision");
        return(NhlFATAL);
      }
      return(NclReturnValue((void*)rvort,ndims_u,dsizes_u,NULL,NCL_float,0));
    }
    else {
/*
 * Return double missing values. 
 */
      for(i = 0; i < total_size_in; i++) dvort[i] = missing;
      return(NclReturnValue((void*)dvort,ndims_u,dsizes_u,NULL,NCL_double,0));
    }
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space.
 * Note the order "vhagc(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork = 2*nlat*(2*nlon*nt+3*l2);
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(br);
  NclFree(bi);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrG","vhagc",&ier,&jer,&ker,&mer,8,5);
/*
 * compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork = nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1);
  ldwork = nlat*(nlat+4);

  work   = (double*)calloc( lwork*sizeof(double),1);
  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);

  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dvrtgc,DVRTGC)(&nlat,&nlon,&isym,&nt,&dvort[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NGCALLF(dchkerr,DCHKERR)("uv2vrG","shsgci+vrtgc",&ier,&jer,&ker,&mer,8,12);

  NclFree(cr);
  NclFree(ci);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;       /* 1/(radius of earth) */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvort[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
/*
 * Return array.
 */
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);
/*
 * Return array.
 */
  if(type_u != NCL_double && type_v != NCL_double){
/*
 * Coerce values back to float if necessary.
 */
    rvort = (float*)calloc(total_size_in*sizeof(float),1);
    if( rvort == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for coercing output to single precision");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_in; i++ ) rvort[i] = (float)dvort[i];
    NclFree(dvort);   /* Free up the double array */
    return(NclReturnValue((void*)rvort,ndims_u,dsizes_u,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dvort,ndims_u,dsizes_u,NULL,NCL_double,0));
  }
}


NhlErrorTypes uv2vrdvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *vr, *dvo;
  double *dvr, *ddv;
  float *rvr, *rdv;
  NclBasicDataTypes type_vr, type_dv;
  int dsizes_vr[NCL_MAX_DIMENSIONS], dsizes_dv[NCL_MAX_DIMENSIONS];
  int ndims_vr, ndims_dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int nt, nlat, nlon, nlatnlon, total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lshsec;
  double *work, *wvhaec, *wshsec, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  vr = (void*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           &type_vr,
           1);
  dvo = (void*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_vr != ndims_u || ndims_dv != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vr[i] != dsizes_u[i] || dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_vr != NCL_float && type_vr != NCL_double) ||
     (type_dv != NCL_float && type_dv != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure vr and dv are double.
 */
  dvr = coerce_output_double(vr,type_vr,total_size_in);
  ddv = coerce_output_double(dvo,type_dv,total_size_in);
  if(dvr == NULL || ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 *  dynamically allocate various temporary space
 *  Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  wvhaec = (double*)calloc(        lvhaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  dwork = (double*)calloc(        ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrdvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhaec'
 */
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  lwork  = max(nlat+1,nlat*(nt*nlon+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat+1;

  wshsec = (double*)calloc(lshsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(ddivec,DDIVEC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NGCALLF(dvrtec,DVRTEC)(&nlat,&nlon,&isym,&nt,&dvr[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);

  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("uv2vrdvf","shseci+divec+vrtec",&ier,&jer,&ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvr[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_vr == NCL_float) rvr = coerce_output_float(dvr,vr,total_size_in,1);
  if(type_dv == NCL_float) rdv = coerce_output_float(ddv,dvo,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes uv2vrdvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing;
/*
 * Output array variables
 */
  void *vr, *dvo;
  double *dvr, *ddv;
  float *rvr, *rdv;
  NclBasicDataTypes type_vr, type_dv;
  int dsizes_vr[NCL_MAX_DIMENSIONS], dsizes_dv[NCL_MAX_DIMENSIONS];
  int ndims_vr, ndims_dv;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  int nt, nlat, nlon, nlatnlon, total_size_in;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lshsgc;
  double *work, *wvhagc, *wshsgc, *br, *bi, *cr, *ci, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           2);
/*
 * Get output arrays.
 */
  vr = (void*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           &type_vr,
           1);
  dvo = (void*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           NULL,
           NULL,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_vr != ndims_u || ndims_dv != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_vr[i] != dsizes_u[i] || dsizes_dv[i] != dsizes_u[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_vr != NCL_float && type_vr != NCL_double) ||
     (type_dv != NCL_float && type_dv != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,has_missing_u,
                           &missing_u,&missing_du,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,has_missing_v,
                           &missing_v,&missing_dv,NULL);
  if( du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure vr and dv are double.
 */
  dvr = coerce_output_double(vr,type_vr,total_size_in);
  ddv = coerce_output_double(dvo,type_dv,total_size_in);
  if(dvr == NULL || ddv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(du,total_size_in,has_missing_u,
                                   missing_du.doubleval);
  found_missing = contains_missing(dv,total_size_in,has_missing_v,
                                   missing_dv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 *  dynamically allocate various temporary space
 *  Note the order "vhaec(...,v,u,....)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhagc = (double*)calloc(        lvhagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  br =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bi =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  cr =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  ci =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  dwork = (double*)calloc(  ldwork*sizeof(double),1);

  if( br == NULL || bi == NULL || cr == NULL || ci == NULL ||
      wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,br,bi,cr,ci,&mdab,&ndab,
                         wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("uv2vrdvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhagc'
 */
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)+2*nt*l1+1));
  ldwork = nlat*(nlat+4);

  wshsgc = (double*)calloc(lshsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);
  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(ddivgc,DDIVGC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,br,bi,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NGCALLF(dvrtgc,DVRTGC)(&nlat,&nlon,&isym,&nt,&dvr[0],&idvw,&jdvw,cr,ci,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);

  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("uv2vrdvg","shsgci+divgc+vrtgc",&ier,&jer,&ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvr[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 1./6.37122e+6;      /* 1/(radius of earth) */

  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&ddv[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_vr == NCL_float) rvr = coerce_output_float(dvr,vr,total_size_in,1);
  if(type_dv == NCL_float) rdv = coerce_output_float(ddv,dvo,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vr2uvf_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *dvort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing;
/*
 * Output array variables
 */
  void *ur, *vr;
  double *dur, *dvr;
  float *rur, *rvr;
  int dsizes_ur[NCL_MAX_DIMENSIONS], dsizes_vr[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ur, type_vr;
  int ndims_ur, ndims_vr;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int lwork, ldwork, lshaec, lvhsec;
  double *work, *wshaec, *wvhsec, *a, *b, *pertrb, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (void*)NclGetArgValue(
           0,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           2);
/*
 * Get output arrays.
 */
  ur = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           NULL,
           NULL,
           &type_ur,
           1);
  vr = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           &type_vr,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_ur != ndims_vort || ndims_vr != ndims_vort ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vort; i++ ) {
    if( dsizes_ur[i] != dsizes_vort[i] || dsizes_vr[i] != dsizes_vort[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_ur != NCL_float && type_ur != NCL_double) ||
     (type_vr != NCL_float && type_vr != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vort,ndims_vort,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce dvort.
 */
  dvort = coerce_input_double(vort,type_vort,total_size_in,has_missing_vort,
                              &missing_vort,&missing_dvort,NULL);
  if(dvort == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dud and dvd are double.
 */
  dur = coerce_output_double(ur,type_ur,total_size_in);
  dvr = coerce_output_double(vr,type_vr,total_size_in);
  if(dur == NULL || dvr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dvort,total_size_in,has_missing_vort,
                                   missing_dvort.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dvort[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dvort[0],
                         &idvw,&jdvw,a,b,&mdab,&ndab,
                         wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vr2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/*
 * reconstruct the divergent (irrotational) wind components
 * note the argument order idivec(...,v,u,...)
 */
  l3     = max(nlat,(nlon+1)/2 );
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  pertrb = (double*)calloc(  nt*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);

  if( pertrb == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(divrtec,DIVRTEC)(&nlat,&nlon,&isym,&nt,&dvr[0],&dur[0],
                           &idvw,&jdvw,a,b,&mdab,&ndab,
                           wvhsec,&lvhsec,work,&lwork,pertrb,&ker);

  NclFree(a);
  NclFree(b);
  NclFree(pertrb);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("vr2uvf","vhseci+ivrtec",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dur[j],&dvr[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dur[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dvort != vort) NclFree(dvort);

  if(type_ur == NCL_float) rur = coerce_output_float(dur,ur,total_size_in,1);
  if(type_vr == NCL_float) rvr = coerce_output_float(dvr,vr,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vr2uvg_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *dvort;
  int ndims_vort, dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing;
/*
 * Output array variables
 */
  void *ur, *vr;
  double *dur, *dvr;
  float *rur, *rvr;
  int dsizes_ur[NCL_MAX_DIMENSIONS], dsizes_vr[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ur, type_vr;
  int ndims_ur, ndims_vr;
/*
 * various
 */
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int lwork, ldwork, lshagc, lvhsgc;
  double *work, *wshagc, *wvhsgc, *a, *b, *pertrb, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (void*)NclGetArgValue(
           0,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           2);
/*
 * Get output arrays.
 */
  ur = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           NULL,
           NULL,
           &type_ur,
           1);
  vr = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           NULL,
           NULL,
           &type_vr,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_ur != ndims_vort || ndims_vr != ndims_vort ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vort; i++ ) {
    if( dsizes_ur[i] != dsizes_vort[i] || dsizes_vr[i] != dsizes_vort[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_ur != NCL_float && type_ur != NCL_double) ||
     (type_vr != NCL_float && type_vr != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vort,ndims_vort,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce dvort.
 */
  dvort = coerce_input_double(vort,type_vort,total_size_in,has_missing_vort,
                              &missing_vort,&missing_dvort,NULL);
  if(dvort == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure dud and dvd are double.
 */
  dur = coerce_output_double(ur,type_ur,total_size_in);
  dvr = coerce_output_double(vr,type_vr,total_size_in);
  if(dur == NULL || dvr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dvort,total_size_in,has_missing_vort,
                                   missing_dvort.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dvort[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  a =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  b =      (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( a == NULL || b == NULL || wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dvort[0],&idvw,&jdvw,
                         a,b,&mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vr2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/*
 * reconstruct the divergent (irrotational) wind components
 * note the argument order idivgc(...,v,u,...)
 */
  l3     = max(nlat,(nlon+1)/2 );
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  pertrb = (double*)calloc( nt*sizeof(double),1);

  if( pertrb == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(divrtgc,DIVRTGC)(&nlat,&nlon,&isym,&nt,&dvr[0],&dur[0],
                           &idvw,&jdvw,a,b,&mdab,&ndab,
                           wvhsgc,&lvhsgc,work,&lwork,pertrb,&ker);

  NclFree(a);
  NclFree(b);
  NclFree(pertrb);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("vr2uvg","vhsgci+ivrtgc",&ier,&jer,&ker,&mer,8,13);
/* 
 * transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
 
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvort[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&dur[j],&dvr[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dur[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dvr[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dvort != vort) NclFree(dvort);

  if(type_ur == NCL_float) rur = coerce_output_float(dur,ur,total_size_in,1);
  if(type_vr == NCL_float) rvr = coerce_output_float(dvr,vr,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvf_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dvo;
  double *dvr, *ddv;
  int ndims_vr, dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv, missing_dvr, missing_ddv;
  NclBasicDataTypes type_vr, type_dv;
  double missing;
  int has_missing_vr, has_missing_dv, found_missing;
/*
 * Output array variables
 */
  void *u, *v;
  double *du, *dv;
  float *ru, *rv;
  NclBasicDataTypes type_u, type_v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec, lvhsec;
  double *work, *wshaec, *wvhsec, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (void*)NclGetArgValue(
           0,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           2);
  dvo = (void*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           2);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_vr || ndims_v != ndims_vr ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_u[i] != dsizes_vr[i] || dsizes_v[i] != dsizes_vr[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce vr and dv.
 */
  dvr = coerce_input_double(vr,type_vr,total_size_in,has_missing_vr,
                            &missing_vr,&missing_dvr,NULL);
  ddv = coerce_input_double(dvo,type_dv,total_size_in,has_missing_dv,
                            &missing_dv,&missing_ddv,NULL);
  if( dvr == NULL || dvr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double.
 */
  du = coerce_output_double(u,type_u,total_size_in);
  dv = coerce_output_double(v,type_v,total_size_in);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dvr,total_size_in,has_missing_vr,
                                   missing_dvr.doubleval);
  found_missing = contains_missing(ddv,total_size_in,has_missing_dv,
                                   missing_ddv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&ddv[j],work);
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dvr[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  ad =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bd =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  av =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bv =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(        ldwork*sizeof(double),1);

  if( ad == NULL || bd == NULL || av == NULL || bv == NULL ||
      wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,ad,bd,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dvr[0],&idvw,&jdvw,av,bv,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);

  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vrdv2uvf","shaec",&ier,&jer,&ker,&mer,10,5);
/* 
 * compute the u and v components fron vr,dv
 */ 
  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon))+nlat*(4*l1*nt+1));
  ldwork = 2*(nlat+2);
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  wvhsec = (double*)calloc(lvhsec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);
  pertbd = (double*)calloc( nt*sizeof(double),1);
  pertbv = (double*)calloc( nt*sizeof(double),1);

  if( pertbd == NULL || pertbv == NULL || wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(didvtec,DIDVTEC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],&idvw,&jdvw,
                           ad,bd,av,bv,&mdab,&ndab,wvhsec,&lvhsec,
                           work,&lwork,pertbd,pertbv,&ker);

  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(pertbd);
  NclFree(pertbv);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("vrdv2uvf","vhseci+idvtec",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvr[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&du[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)ddv != dvo) NclFree(ddv);
  if((void*)dvr != vr) NclFree(dvr);

  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_in,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvg_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dvo;
  double *dvr, *ddv;
  int ndims_vr, dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv, dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv, missing_dvr, missing_ddv;
  NclBasicDataTypes type_vr, type_dv;
  double missing;
  int has_missing_vr, has_missing_dv, found_missing;
/*
 * Output array variables
 */
  void *u, *v;
  double *du, *dv;
  float *ru, *rv;
  NclBasicDataTypes type_u, type_v;
  int dsizes_u[NCL_MAX_DIMENSIONS], dsizes_v[NCL_MAX_DIMENSIONS];
  int ndims_u, ndims_v;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in;
  int i, j, l, isym, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0, ner=0;
  double scale;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc, lvhsgc;
  double *work, *wshagc, *wvhsgc, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (void*)NclGetArgValue(
           0,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           2);
  dvo = (void*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           2);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same size.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_vr || ndims_v != ndims_vr ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_u[i] != dsizes_vr[i] || dsizes_v[i] != dsizes_vr[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce vr and dv.
 */
  dvr = coerce_input_double(vr,type_vr,total_size_in,has_missing_vr,
                            &missing_vr,&missing_dvr,NULL);
  ddv = coerce_input_double(dvo,type_dv,total_size_in,has_missing_dv,
                            &missing_dv,&missing_ddv,NULL);
  if( dvr == NULL || dvr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double.
 */
  du = coerce_output_double(u,type_u,total_size_in);
  dv = coerce_output_double(v,type_v,total_size_in);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  found_missing = contains_missing(dvr,total_size_in,has_missing_vr,
                                   missing_dvr.doubleval);
  found_missing = contains_missing(ddv,total_size_in,has_missing_dv,
                                   missing_ddv.doubleval);
  if(found_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input array cannot contain any missing values");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&ddv[j],work);
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dvr[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nt*nlon+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  ad =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bd =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  av =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  bv =     (double*)calloc(  mdab*ndab*nt*sizeof(double),1);
  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( ad == NULL || bd == NULL || av == NULL || bv == NULL ||
      wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&ddv[0],&idvw,&jdvw,ad,bd,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dvr[0],&idvw,&jdvw,av,bv,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vrdv2uvg","shagc",&ier,&jer,&ker,&mer,10,5);
/* 
 * compute the u and v components fron vr,dv
 */ 
  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+4*nt*l1+1));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (double*)calloc(lvhsgc*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);
  pertbd = (double*)calloc( nt*sizeof(double),1);
  pertbv = (double*)calloc( nt*sizeof(double),1);

  if( pertbd == NULL || pertbv == NULL || wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(didvtgc,DIDVTGC)(&nlat,&nlon,&isym,&nt,&dv[0],&du[0],&idvw,&jdvw,
                           ad,bd,av,bv,&mdab,&ndab,wvhsgc,&lvhsgc,
                           work,&lwork,pertbd,pertbv,&ker);

  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(pertbd);
  NclFree(pertbv);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("vrdv2uvg","vhsgci+idvtgc",&ier,&jer,&ker,&mer,10,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&ddv[j],work);
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dvr[j],work);
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * (possibly) scale the quantities calculated by this routine
 */
  scale = 6.37122e+6;         /* radius of earth */
  
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&du[0],&scale,&ner);
  NGCALLF(dgeoscl,DGEOSCL)(&nlon,&nlat,&nt,&dv[0],&scale,&ner);
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)ddv != dvo) NclFree(ddv);
  if((void*)dvr != vr) NclFree(dvr);

  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_in,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_in,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhaec_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void    *br,  *bi,  *cr,  *ci;
  double *dbr, *dbi, *dcr, *dci;
  float  *rbr, *rbi, *rcr, *rci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhaec, lvhsec;
  double *work, *wvhaec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           6,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           2);
/*
 * Get in/output arrays.
 */
  br = (void*)NclGetArgValue(
           2,
           6,
           &ndims_br,
           dsizes_br,
           NULL,
           NULL,
           &type_br,
           1);
  bi = (void*)NclGetArgValue(
           3,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           1);
  cr = (void*)NclGetArgValue(
           4,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           1);
  ci = (void*)NclGetArgValue(
           5,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be the same size.
 */
  if( ndims_br != ndims_u || ndims_bi != ndims_u  ||
      ndims_cr != ndims_u || ndims_ci != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u-2; i++ ) {
    if( dsizes_br[i] != dsizes_u[i] || dsizes_bi[i] != dsizes_u[i] ||
        dsizes_cr[i] != dsizes_u[i] || dsizes_ci[i] != dsizes_u[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input/output arrays must have the same leftmost dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2]) ||
     (dsizes_br[ndims_br-2] != dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The last two dimensions of the output coefficient arrays must be the same size and have the same number of latitude points as the input grid");
    return(NhlFATAL);
  }
  if((type_br != NCL_float && type_br != NCL_double) ||
     (type_bi != NCL_float && type_bi != NCL_double) ||
     (type_cr != NCL_float && type_cr != NCL_double) ||
     (type_ci != NCL_float && type_ci != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_out = nt * nlat * nlat;
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure br, bi, cr, ci are double
 */
  dbr = coerce_output_double(br,type_br,total_size_out);
  dbi = coerce_output_double(bi,type_bi,total_size_out);
  dcr = coerce_output_double(cr,type_cr,total_size_out);
  dci = coerce_output_double(ci,type_ci,total_size_out);
  if(dbr == NULL || dbi == NULL || dcr == NULL || dci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space
 * Note the order "vhaec(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  wvhaec = (double*)calloc(lvhaec*sizeof(double),1);
  work   = (double*)calloc( lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhaeci,DVHAECI)(&nlat,&nlon,wvhaec,&lvhaec,dwork,&ldwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,dbr,dbi,dcr,dci,
                         &mdab,&ndab,wvhaec,&lvhaec,work,&lwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhaec","vhaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_br == NCL_float) rbr = coerce_output_float(dbr,br,total_size_out,1);
  if(type_bi == NCL_float) rbi = coerce_output_float(dbi,bi,total_size_out,1);
  if(type_cr == NCL_float) rcr = coerce_output_float(dcr,cr,total_size_out,1);
  if(type_ci == NCL_float) rci = coerce_output_float(dci,ci,total_size_out,1);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes vhagc_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void    *br,  *bi,  *cr,  *ci;
  double *dbr, *dbi, *dcr, *dci;
  float  *rbr, *rbi, *rcr, *rci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhagc, lvhsec;
  double *work, *wvhagc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  u = (void*)NclGetArgValue(
           0,
           6,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           2);
  v = (void*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           2);
/*
 * Get in/output arrays.
 */
  br = (void*)NclGetArgValue(
           2,
           6,
           &ndims_br,
           dsizes_br,
           NULL,
           NULL,
           &type_br,
           1);
  bi = (void*)NclGetArgValue(
           3,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           1);
  cr = (void*)NclGetArgValue(
           4,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           1);
  ci = (void*)NclGetArgValue(
           5,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_br != ndims_u || ndims_bi != ndims_u  ||
      ndims_cr != ndims_u || ndims_ci != ndims_u ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u-2; i++ ) {
    if( dsizes_br[i] != dsizes_u[i] || dsizes_bi[i] != dsizes_u[i] ||
        dsizes_cr[i] != dsizes_u[i] || dsizes_ci[i] != dsizes_u[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input/output arrays must have the same leftmost dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2]) ||
     (dsizes_br[ndims_br-2] != dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The last two dimensions of the output coefficient arrays must be the same size and have the same number of latitude points as the input grid");
    return(NhlFATAL);
  }
  if((type_br != NCL_float && type_br != NCL_double) ||
     (type_bi != NCL_float && type_bi != NCL_double) ||
     (type_cr != NCL_float && type_cr != NCL_double) ||
     (type_ci != NCL_float && type_ci != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_out = nt * nlat * nlat;
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure br, bi, cr, ci are double
 */
  dbr = coerce_output_double(br,type_br,total_size_out);
  dbi = coerce_output_double(bi,type_bi,total_size_out);
  dcr = coerce_output_double(cr,type_cr,total_size_out);
  dci = coerce_output_double(ci,type_ci,total_size_out);
  if(dbr == NULL || dbi == NULL || dcr == NULL || dci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&nlon,&nlat,&du[j],&dv[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * perform vector spherical harmonic analysis to get coefficients 
 * dynamically allocate various temporary space
 * Note the order "vhagc(...,v,u,....)
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt +3*l2 ));
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhagc = (double*)calloc( lvhagc*sizeof(double),1);
  work =   (double*)calloc(  lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhagci,DVHAGCI)(&nlat,&nlon,wvhagc,&lvhagc,dwork,&ldwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,dbr,dbi,dcr,dci,
                         &mdab,&ndab,wvhagc,&lvhagc,work,&lwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhagc","vhagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

  if(type_br == NCL_float) rbr = coerce_output_float(dbr,br,total_size_out,1);
  if(type_bi == NCL_float) rbi = coerce_output_float(dbi,bi,total_size_out,1);
  if(type_cr == NCL_float) rcr = coerce_output_float(dcr,cr,total_size_out,1);
  if(type_ci == NCL_float) rci = coerce_output_float(dci,ci,total_size_out,1);
/*
 * Return
 */
  return(NhlNOERROR);
}

NhlErrorTypes vhsec_W( void )
{
/*
 * Input array variables
 */
  void *br, *bi, *cr, *ci;
  double *dbr, *dbi, *dcr, *dci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * Output array variables
 */
  void    *u,  *v;
  double *du, *dv;
  float  *ru, *rv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhsec;
  double *work, *wvhsec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  br = (void*)NclGetArgValue(
           0,
           6,
           &ndims_br,
           dsizes_br,
           NULL,
           NULL,
           &type_br,
           2);
  bi = (void*)NclGetArgValue(
           1,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           2);
  cr = (void*)NclGetArgValue(
           2,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           2);
  ci = (void*)NclGetArgValue(
           3,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           2);
/*
 * Get in/output arrays.
 */
  u = (void*)NclGetArgValue(
           4,
           6,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           5,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_br != ndims_bi || ndims_br != ndims_ci || ndims_br != ndims_cr ||
      ndims_bi != ndims_ci || ndims_bi != ndims_cr ||
      ndims_cr != ndims_ci || ndims_br < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br; i++ ) {
    if( dsizes_br[i] != dsizes_bi[i] || dsizes_br[i] != dsizes_ci[i] ||
        dsizes_br[i] != dsizes_cr[i] || dsizes_bi[i] != dsizes_cr[i] ||
        dsizes_bi[i] != dsizes_ci[i] || dsizes_ci[i] != dsizes_cr[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The last two dimensions of input coefficient arrays must be the same size and have the same number of latitude points as the output grid");
    return(NhlFATAL);
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_out);
  total_size_in = nt * nlat * nlat;
/*
 * Coerce br, bi, cr, ci.
 */
  dbr = coerce_input_double(br,type_br,total_size_in,0,NULL,NULL,NULL);
  dbi = coerce_input_double(bi,type_bi,total_size_in,0,NULL,NULL,NULL);
  dcr = coerce_input_double(cr,type_cr,total_size_in,0,NULL,NULL,NULL);
  dci = coerce_input_double(ci,type_ci,total_size_in,0,NULL,NULL,NULL);
  if(dbr == NULL || dbi == NULL || dcr == NULL || dci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double
 */
  du = coerce_output_double(u,type_u,total_size_out);
  dv = coerce_output_double(v,type_v,total_size_out);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*(nlat+1),nlat*(2*nt*nlon+max(6*l2,nlon)));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (double*)calloc(        lvhsec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc( ldwork*sizeof(double),1);

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhseci,DVHSECI)(&nlat,&nlon,wvhsec,&lvhsec,dwork,&ldwork,&jer);
  NGCALLF(dvhsec,DVHSEC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,dbr,dbi,dcr,dci,&mdab,&ndab,
                         wvhsec,&lvhsec,work,&lwork,&ker);
  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhsec","vhsec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dbr != br) NclFree(dbr);
  if((void*)dbi != bi) NclFree(dbi);
  if((void*)dcr != cr) NclFree(dcr);
  if((void*)dci != ci) NclFree(dci);
/*
 * Return
 */
  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_out,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes vhsgc_W( void )
{
/*
 * Input array variables
 */
  void *br, *bi, *cr, *ci;
  double *dbr, *dbi, *dcr, *dci;
  int ndims_br, dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi, dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr, dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci, dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * Output array variables
 */
  void    *u,  *v;
  double *du, *dv;
  float  *ru, *rv;
  int ndims_u, dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v, dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, ityp, idvw, jdvw, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lvhsgc;
  double *work, *wvhsgc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  br = (void*)NclGetArgValue(
           0,
           6,
           &ndims_br,
           dsizes_br,
           NULL,
           NULL,
           &type_br,
           2);
  bi = (void*)NclGetArgValue(
           1,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           2);
  cr = (void*)NclGetArgValue(
           2,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           2);
  ci = (void*)NclGetArgValue(
           3,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           2);
/*
 * Get in/output arrays.
 */
  u = (void*)NclGetArgValue(
           4,
           6,
           &ndims_u, 
           dsizes_u,
           NULL,
           NULL,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           5,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if( ndims_br != ndims_bi || ndims_br != ndims_ci || ndims_br != ndims_cr ||
      ndims_bi != ndims_ci || ndims_bi != ndims_cr ||
      ndims_cr != ndims_ci || ndims_br < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br; i++ ) {
    if( dsizes_br[i] != dsizes_bi[i] || dsizes_br[i] != dsizes_ci[i] ||
        dsizes_br[i] != dsizes_cr[i] || dsizes_bi[i] != dsizes_cr[i] ||
        dsizes_bi[i] != dsizes_ci[i] || dsizes_ci[i] != dsizes_cr[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((dsizes_br[ndims_br-1] != dsizes_br[ndims_br-2])||(dsizes_br[ndims_br-2]!=dsizes_u[ndims_u-2])) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The last two dimensions of input coefficient arrays must be the same size and have the same number of latitude points as the output grid");
    return(NhlFATAL);
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_out);
  total_size_in = nt * nlat * nlat;
/*
 * Coerce br, bi, cr, ci.
 */
  dbr = coerce_input_double(br,type_br,total_size_in,0,NULL,NULL,NULL);
  dbi = coerce_input_double(bi,type_bi,total_size_in,0,NULL,NULL,NULL);
  dcr = coerce_input_double(cr,type_cr,total_size_in,0,NULL,NULL,NULL);
  dci = coerce_input_double(ci,type_ci,total_size_in,0,NULL,NULL,NULL);
  if(dbr == NULL || dbi == NULL || dcr == NULL || dci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure u and v are double
 */
  du = coerce_output_double(u,type_u,total_size_out);
  dv = coerce_output_double(v,type_v,total_size_out);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = nlat;
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)));
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  ldwork = 2*nlat*(nlat+1)+1;

  wvhsgc = (double*)calloc(        lvhsgc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dvhsgci,DVHSGCI)(&nlat,&nlon,wvhsgc,&lvhsgc,dwork,&ldwork,&jer);
  NGCALLF(dvhsgc,DVHSGC)(&nlat,&nlon,&ityp,&nt,&dv[0],&du[0],
                         &idvw,&jdvw,dbr,dbi,dcr,dci,&mdab,&ndab,
                         wvhsgc,&lvhsgc,work,&lwork,&ker);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhsgc","vhsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&nlat,&nlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dbr != br) NclFree(dbr);
  if((void*)dbi != bi) NclFree(dbi);
  if((void*)dcr != cr) NclFree(dcr);
  if((void*)dci != ci) NclFree(dci);
/*
 * Return
 */
  if(type_u == NCL_float) ru = coerce_output_float(du,u,total_size_out,1);
  if(type_v == NCL_float) rv = coerce_output_float(dv,v,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes shaec_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  void *a, *b;
  double *da, *db;
  float *ra, *rb;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int lwork, ldwork, lshaec;
  double *work, *wshaec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (void*)NclGetArgValue(
          0,
          3,
          &ndims_g,
          dsizes_g,
          NULL,
          NULL,
          &type_g,
          2);
/*
 * Get coefficient arrays.
 */
  a = (void*)NclGetArgValue(
           1,
           3,
           &ndims_a, 
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           1);
  b = (void*)NclGetArgValue(
           2,
           3,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must have same dimensionality as input arrays, except
 * for last two dimensions.
 */
  if( ndims_a != ndims_g || ndims_b != ndims_g ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_g-2; i++ ) {
    if( dsizes_a[i] != dsizes_g[i] || dsizes_b[i] != dsizes_g[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  total_size_out = nt * ndab * mdab;
/*
 * Check last two dimensions of a and b.
 */
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The last two dimensions of the output coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Coerce g.
 */
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure a and b are double
 */
  da = coerce_output_double(a,type_a,total_size_out);
  db = coerce_output_double(b,type_b,total_size_out);
  if(da == NULL || db == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,dg,&idg,&jdg,da,db,
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shaec","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dg != g) NclFree(dg);
/*
 * Return
 */
  if(type_a == NCL_float) ra = coerce_output_float(da,a,total_size_out,1);
  if(type_b == NCL_float) rb = coerce_output_float(db,b,total_size_out,1);

  return(NhlNOERROR);
}

NhlErrorTypes shagc_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  void *a, *b;
  double *da, *db;
  float *ra, *rb;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int lwork, ldwork, lshagc;
  double *work, *wshagc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (void*)NclGetArgValue(
          0,
          3,
          &ndims_g,
          dsizes_g,
          NULL,
          NULL,
          &type_g,
          2);
/*
 * Get coefficient arrays.
 */
  a = (void*)NclGetArgValue(
           1,
           3,
           &ndims_a, 
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           1);
  b = (void*)NclGetArgValue(
           2,
           3,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           1);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * The output arrays must have same dimensionality as input arrays, except
 * for last two dimensions.
 */
  if( ndims_a != ndims_g || ndims_b != ndims_g ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_g-2; i++ ) {
    if( dsizes_a[i] != dsizes_g[i] || dsizes_b[i] != dsizes_g[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  total_size_out = nt * ndab * mdab;
/*
 * Check last two dimensions of a and b.
 */
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The last two dimensions of the output coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Coerce g.
 */
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure a and b are double
 */
  da = coerce_output_double(a,type_a,total_size_out);
  db = coerce_output_double(b,type_b,total_size_out);
  if(da == NULL || db == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for double precision output arrays");
    return(NhlFATAL);
  }
/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,dg,&idg,&jdg,da,db,
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shagc","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dg != g) NclFree(dg);
/*
 * Return
 */
  if(type_a == NCL_float) ra = coerce_output_float(da,a,total_size_out,1);
  if(type_b == NCL_float) rb = coerce_output_float(db,b,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes shsec_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *da, *db;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *g;
  double *dg;
  float *rg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int lwork, ldwork, lshsec;
  double *work, *wshsec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
           0,
           3,
           &ndims_a, 
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           2);
  b = (void*)NclGetArgValue(
           1,
           3,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           2);
/*
 * Get output array.
 */
  g = (void*)NclGetArgValue(
          2,
          3,
          &ndims_g,
          dsizes_g,
          NULL,
          NULL,
          &type_g,
          1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if( dsizes_a[i] != dsizes_b[i] ){
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_g != ndims_a ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a-2; i++ ) {
    if( dsizes_g[i] != dsizes_a[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
  if((type_g != NCL_float && type_g != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Check last two dimensions of a and b.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_out);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The last two dimensions of the coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
  total_size_in = nt * ndab * mdab;
/*
 * Coerce a and b.
 */
  da = coerce_input_double(a,type_a,total_size_in,0,NULL,NULL,NULL);
  db = coerce_input_double(b,type_b,total_size_in,0,NULL,NULL,NULL);
  if(da == NULL || db == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure g is double
 */
  dg = coerce_output_double(g,type_g,total_size_out);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Calculate work space sizes.
 */
  isym   = 0; 
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = nlat*(nlon*nt+max(3*l2,nlon));
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshsec = (double*)calloc(        lshsec*sizeof(double),1);
  work   = (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshseci,DSHSECI)(&nlat,&nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  NGCALLF(dshsec,DSHSEC)(&nlat,&nlon,&isym,&nt,&dg[0],&idg,&jdg,da,db,
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shsec","shseci+shsec",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)da != a) NclFree(da);
  if((void*)db != b) NclFree(db);
/*
 * Return
 */
  if(type_g == NCL_float) rg = coerce_output_float(dg,g,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes shsgc_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *da, *db;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *g;
  double *dg;
  float *rg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * various
 */
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int lwork, ldwork, lshsgc;
  double *work, *wshsgc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
           0,
           3,
           &ndims_a, 
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           2);
  b = (void*)NclGetArgValue(
           1,
           3,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           2);
/*
 * Get output array.
 */
  g = (void*)NclGetArgValue(
          2,
          3,
          &ndims_g,
          dsizes_g,
          NULL,
          NULL,
          &type_g,
          1);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input arrays must be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
    if( dsizes_a[i] != dsizes_b[i] ){
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_g != ndims_a ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a-2; i++ ) {
    if( dsizes_g[i] != dsizes_a[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The first n-2 dimensions of the input/output arrays must be the same size");
      return(NhlFATAL);
    }
  }
  if((type_g != NCL_float && type_g != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * Check last two dimensions of a and b.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_out);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_a[ndims_a-1] != mdab || dsizes_a[ndims_a-2] != ndab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The last two dimensions of the coefficient arrays must be nlat x min(nlat,(nlon+2)/2) if nlon is even or nlat x min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
  total_size_in = nt * ndab * mdab;
/*
 * Coerce a and b.
 */
  da = coerce_input_double(a,type_a,total_size_in,0,NULL,NULL,NULL);
  db = coerce_input_double(b,type_b,total_size_in,0,NULL,NULL,NULL);
  if(da == NULL || db == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure g is double
 */
  dg = coerce_output_double(g,type_g,total_size_out);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for double precision output array");
    return(NhlFATAL);
  }
/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  wshsgc = (double*)calloc(        lshsgc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshsgci,DSHSGCI)(&nlat,&nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  NGCALLF(dshsgc,DSHSGC)(&nlat,&nlon,&isym,&nt,&dg[0],&idg,&jdg,da,db,
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shsgc","shsgci+shsgc",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)da != a) NclFree(da);
  if((void*)db != b) NclFree(db);
/*
 * Return
 */
  if(type_g == NCL_float) rg = coerce_output_float(dg,g,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes shaeC_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  double *dab;
  float  *rab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshaec;
  double *work, *wshaec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (void*)NclGetArgValue(
             0,
             1,
             &ndims_g,
             dsizes_g,
             NULL,
             NULL,
             &type_g,
             2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  total_size_out = 2 * ndab * mdab * nt;
/*
 * Coerce g.
 */
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  dab  = (double*)calloc(total_size_out*sizeof(double),1);
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshaec = (double*)calloc(        lshaec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshaeci,DSHAECI)(&nlat,&nlon,wshaec,&lshaec,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshaec,DSHAEC)(&nlat,&nlon,&isym,&nt,&dg[0],&idg,&jdg,
                         &dab[0],&dab[j],
                         &mdab,&ndab,wshaec,&lshaec,work,&lwork,&ker);
  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shaeC","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dg != g) NclFree(dg);
/*
 * Return
 */
  ndims_ab = ndims_g + 1;
  dsizes_ab[0] = 2;
  for( i = 1; i <= ndims_ab-2; i++ ) dsizes_ab[i] = dsizes_g[i-1];
  dsizes_ab[ndims_ab-1] = mdab;
/*
 * Determine whether to return float or double.
 */
  if(type_g != NCL_double) {
    rab = (float*)NclMalloc(total_size_out*sizeof(float));
    if (rab == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rab[i] = (float)dab[i];
/*
 * Free double precision array.
 */
    NclFree(dab);

    return(NclReturnValue((void*)rab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dab,ndims_ab,dsizes_ab,NULL,NCL_double,0));
  }
}

NhlErrorTypes shagC_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  double *dab;
  float  *rab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshagc;
  double *work, *wshagc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  g = (void*)NclGetArgValue(
           0,
           1,
           &ndims_g,
           dsizes_g,
           NULL,
           NULL,
           &type_g,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_g < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  compute_nlatnlon(dsizes_g,ndims_g,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  ndab   = nlat;
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  total_size_out = 2 * ndab * mdab * nt;
/*
 * Coerce g.
 */
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  dab  = (double*)calloc(total_size_out*sizeof(double),1);
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work = (double*)calloc(lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&nlon,&nlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (double*)calloc(        lshagc*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork =  (double*)calloc(       ldwork*sizeof(double),1);

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshagci,DSHAGCI)(&nlat,&nlon,wshagc,&lshagc,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshagc,DSHAGC)(&nlat,&nlon,&isym,&nt,&dg[0],&idg,&jdg,
                         &dab[0],&dab[j],
                         &mdab,&ndab,wshagc,&lshagc,work,&lwork,&ker);
  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shagC","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,&nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dg != g) NclFree(dg);
/*
 * Return
 */
  ndims_ab = ndims_g + 1;
  dsizes_ab[0] = 2;
  for( i = 1; i <= ndims_ab-2; i++ ) dsizes_ab[i] = dsizes_g[i-1];
  dsizes_ab[ndims_ab-1] = mdab;
/*
 * Determine whether to return float or double.
 */
  if(type_g != NCL_double) {
    rab = (float*)NclMalloc(total_size_out*sizeof(float));
    if (rab == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rab[i] = (float)dab[i];
/*
 * Free double precision array.
 */
    NclFree(dab);

    return(NclReturnValue((void*)rab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dab,ndims_ab,dsizes_ab,NULL,NCL_double,0));
  }
}

NhlErrorTypes shseC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *dab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab;
/*
 * Output array variables
 */
  double *dg;
  float *rg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int nt, nlat, *nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsec;
  double *work, *wshsec, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (void*)NclGetArgValue(
           0,
           2,
           &ndims_ab, 
           dsizes_ab,
           NULL,
           NULL,
           &type_ab,
           2);
/*
 * Get nlon
 */
  nlon = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Check the last dimension of ab.
 */
  ndab = nlat = dsizes_ab[ndims_ab-2];
  if (*nlon % 2) {
    mdab   = min(nlat,(*nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(*nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nlatnlon = *nlon * nlat;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_in  = 2 * nt * mdab * ndab;
  total_size_out = nt * *nlon * nlat;
/*
 * Coerce ab.
 */
  dab = coerce_input_double(ab,type_ab,total_size_in,0,NULL,NULL,NULL);
  if(dab == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }

/*
 * Compute size for output array.
 */
  dg = (double *)calloc(total_size_out*sizeof(double),1);
  if( dg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = *nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = nlat*(*nlon*nt+max(3*l2,*nlon));
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+ *nlon +15;
  ldwork = nlat+1;

  wshsec = (double*)calloc(        lshsec*sizeof(double),1);
  work =   (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshseci,DSHSECI)(&nlat,nlon,wshsec,&lshsec,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshsec,DSHSEC)(&nlat,nlon,&isym,&nt,&dg[0],&idg,&jdg,
                         &dab[0],&dab[j],
                         &mdab,&ndab,wshsec,&lshsec,work,&lwork,&ker);
  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shseC","shseci+shsec",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dab != ab) NclFree(dab);
/*
 * Return
 */
  ndims_g = ndims_ab-1;
  for( i = 0; i <= ndims_g-2; i++ ) dsizes_g[i] = dsizes_ab[i+1];
  dsizes_g[ndims_g-1] = *nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_ab != NCL_double) {
    rg = (float*)NclMalloc(total_size_out*sizeof(float));
    if (rg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rg[i] = (float)dg[i];
/*
 * Free double precision array.
 */
    NclFree(dg);

    return(NclReturnValue((void*)rg,ndims_g,dsizes_g,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dg,ndims_g,dsizes_g,NULL,NCL_double,0));
  }
}


NhlErrorTypes shsgC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *dab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab;
/*
 * Output array variables
 */
  double *dg;
  float *rg;
  int ndims_g, dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int nt, nlat, *nlon, nlatnlon, total_size_in, total_size_out;
  int i, j, l, isym, idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  int lwork, ldwork, lshsgc;
  double *work, *wshsgc, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (void*)NclGetArgValue(
           0,
           2,
           &ndims_ab, 
           dsizes_ab,
           NULL,
           NULL,
           &type_ab,
           2);
/*
 * Get nlon
 */
  nlon = (int*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The grid coming in must be at least 3-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Check the last dimension of ab.
 */
  ndab = nlat = dsizes_ab[ndims_ab-2];
  if (*nlon % 2) {
    mdab   = min(nlat,(*nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(*nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nlatnlon = *nlon * nlat;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_in  = 2 * nt * mdab * ndab;
  total_size_out = nt * *nlon * nlat;
/*
 * Coerce ab.
 */
  dab = coerce_input_double(ab,type_ab,total_size_in,0,NULL,NULL,NULL);
  if(dab == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Compute size for output array.
 */
  dg = (double *)calloc(total_size_out*sizeof(double),1);
  if( dg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = *nlon;
  l1     = mdab;
  l2     = (nlat+1)/2;

  lwork  = max(4*nlat*(nlat+2)+2,nlat*(*nlon*nt+max(3*l2,*nlon)));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+ *nlon +15;

  wshsgc = (double*)calloc(        lshsgc*sizeof(double),1);
  work   = (double*)calloc(         lwork*sizeof(double),1);
  dwork  = (double*)calloc(ldwork*sizeof(double),1);

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  NGCALLF(dshsgci,DSHSGCI)(&nlat,nlon,wshsgc,&lshsgc,dwork,&ldwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshsgc,DSHSGC)(&nlat,nlon,&isym,&nt,&dg[0],&idg,&jdg,
                         &dab[0],&dab[j],
                         &mdab,&ndab,wshsgc,&lshsgc,work,&lwork,&ker);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shsgC","shsgci+shsgc",&ier,&jer,&ker,&mer,5,12);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work   = (double*)calloc( lwork*sizeof(double),1);
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&nlat,nlon,&dg[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)dab != ab) NclFree(dab);
/*
 * Return
 */
  ndims_g = ndims_ab-1;
  for( i = 0; i <= ndims_g-2; i++ ) dsizes_g[i] = dsizes_ab[i+1];
  dsizes_g[ndims_g-1] = *nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_ab != NCL_double) {
    rg = (float*)NclMalloc(total_size_out*sizeof(float));
    if (rg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rg[i] = (float)dg[i];
/*
 * Free double precision array.
 */
    NclFree(dg);

    return(NclReturnValue((void*)rg,ndims_g,dsizes_g,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dg,ndims_g,dsizes_g,NULL,NCL_double,0));
  }
}




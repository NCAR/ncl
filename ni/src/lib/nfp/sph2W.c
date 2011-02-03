#include <stdio.h>
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

extern void NGCALLF(disfvpec,DISFVPEC)(int *,int *,int *,int *,double
*,double *, int *,int *,double *,double *,double *,double *,int *,int *,
double *,int *,double *,int *,int *);

extern void NGCALLF(disfvpgc,DISFVPGC)(int *,int *,int *,int *,double
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

extern void NGCALLF(dshsgcr42,DSHSGCR42)(double *,double *,double *,double *,
					 int *);

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

double scale = 6.37122e+6;         /* radius of earth */

NhlErrorTypes dv2uvf_W( void )
{
/*
 * Input array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing_dv;
/*
 * Output array variables
 */
  void *ud = NULL;
  void *vd = NULL;
  double *tmp_ud = NULL;
  double *tmp_vd = NULL;
  int ndims_ud;
  ng_size_t dsizes_ud[NCL_MAX_DIMENSIONS];
  int ndims_vd;
  ng_size_t dsizes_vd[NCL_MAX_DIMENSIONS];
  int has_missing_ud, has_missing_vd;
  NclScalar missing_ud, missing_vd, missing_dud, missing_dvd;
  NclBasicDataTypes type_ud, type_vd;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_dv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *pertrb, *a, *b;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  ud = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           &missing_ud,
           &has_missing_ud,
           &type_ud,
           1);
  vd = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           &missing_vd,
           &has_missing_vd,
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
 * The output arrays must be float or double, and input/output arrays 
 * must be 2 or 3-dimensional.
 */
  if((type_ud != NCL_float && type_ud != NCL_double) ||
     (type_vd != NCL_float && type_vd != NCL_double)) {
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
 * Coerce the missing values.
 */
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,NULL);
  coerce_missing(type_ud,has_missing_ud,&missing_ud,&missing_dud,NULL);
  coerce_missing(type_vd,has_missing_vd,&missing_vd,&missing_dvd,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_dv is just big enough to hold a 2-dimensional subsection of the
 * dv array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv.
 */
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ud != NCL_double) {
    tmp_ud = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ud == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for coercing ud array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_vd != NCL_double) {
    tmp_vd = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for coercing vd array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)+2*l1+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshaec == NULL || wvhsec == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvf: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;

/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_dv = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_dv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_dv];
    }
    if(type_ud == NCL_double) {
/*
 * Point tmp_ud to appropriate location in ud.
 */
      tmp_ud = &((double*)ud)[index_dv];
    }
    if(type_vd == NCL_double) {
/*
 * Point tmp_vd to appropriate location in vd.
 */
      tmp_vd = &((double*)vd)[index_dv];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    if(found_missing_dv) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_ud) {
        set_subset_output_missing(ud,index_dv,type_ud,nlatnlon,
                                  missing_dud.doubleval);
      }
      if(has_missing_vd) {
        set_subset_output_missing(vd,index_dv,type_vd,nlatnlon,
                                  missing_dvd.doubleval);
      }
    }
    else {
/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvf","shaec",&ier,&jer,&ker,&mer,6,5);
/* 
 * Reconstruct the divergent (irrotational) wind components.
 * Note the argument order idivec(...,vd,ud,...)
 */
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(didivec,DIDIVEC)(&inlat,&inlon,&isym,&one,tmp_vd,tmp_ud,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsec,&ilvhsec,
			       work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvf","vhseci,idivec",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ud,tmp_vd,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ud,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vd,&scale,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_ud == NCL_float) {
	coerce_output_float_only(ud,tmp_ud,nlatnlon,index_dv);
      }
      if(type_vd == NCL_float) {
	coerce_output_float_only(vd,tmp_vd,nlatnlon,index_dv);
      }
    }
    index_dv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dv2uvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_ud != NCL_double) NclFree(tmp_ud);
  if(type_vd != NCL_double) NclFree(tmp_vd);

  return(NhlNOERROR);
}

NhlErrorTypes dv2uvg_W( void )
{
/*
 * Input array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing_dv;
/*
 * Output array variables
 */
  void *ud = NULL;
  void *vd = NULL;
  double *tmp_ud = NULL;
  double *tmp_vd = NULL;
  int ndims_ud;
  ng_size_t dsizes_ud[NCL_MAX_DIMENSIONS];
  int ndims_vd;
  ng_size_t dsizes_vd[NCL_MAX_DIMENSIONS];
  int has_missing_ud, has_missing_vd;
  NclScalar missing_ud, missing_vd, missing_dud, missing_dvd;
  NclBasicDataTypes type_ud, type_vd;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_dv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *pertrb, *a, *b;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  ud = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ud, 
           dsizes_ud,
           &missing_ud,
           &has_missing_ud,
           &type_ud,
           1);
  vd = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vd, 
           dsizes_vd,
           &missing_vd,
           &has_missing_vd,
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
 * The output arrays must be float or double, and input/output arrays 
 * must be 2 or 3-dimensional.
 */
  if((type_ud != NCL_float && type_ud != NCL_double) ||
     (type_vd != NCL_float && type_vd != NCL_double)) {
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
 * Coerce the missing values.
 */
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,NULL);
  coerce_missing(type_ud,has_missing_ud,&missing_ud,&missing_dud,NULL);
  coerce_missing(type_vd,has_missing_vd,&missing_vd,&missing_dvd,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_dv is just big enough to hold a 2-dimensional subsection of the
 * dv array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv.
 */
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ud != NCL_double) {
    tmp_ud = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ud == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for coercing ud array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vd != NCL_double) {
    tmp_vd = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for coercing vd array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = 10*(max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon))));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon)+2*l1+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
     
  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshagc == NULL || wvhsgc == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
     
/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvg: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_dv = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_dv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_dv];
    }
    if(type_ud == NCL_double) {
/*
 * Point tmp_ud to appropriate location in ud.
 */
      tmp_ud = &((double*)ud)[index_dv];
    }
    if(type_vd == NCL_double) {
/*
 * Point tmp_vd to appropriate location in vd.
 */
      tmp_vd = &((double*)vd)[index_dv];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    if(found_missing_dv) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_ud) {
        set_subset_output_missing(ud,index_dv,type_ud,nlatnlon,
                                  missing_dud.doubleval);
      }
      if(has_missing_vd) {
        set_subset_output_missing(vd,index_dv,type_vd,nlatnlon,
                                  missing_dvd.doubleval);
      }
    }
    else {
/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvg","shagc",&ier,&jer,&ker,&mer,6,5);

/* 
 * Reconstruct the divergent (irrotational) wind components.
 * note the argument order idivgc(...,vd,ud,...)
 */
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,
			       &jer);
      NGCALLF(didivgc,DIDIVGC)(&inlat,&inlon,&isym,&one,tmp_vd,tmp_ud,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsgc,&ilvhsgc,
			       work3,&ilwork3,pertrb,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("dv2uvg","vhsgci,idivgc",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ud,tmp_vd,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ud,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vd,&scale,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_ud == NCL_float) {
	coerce_output_float_only(ud,tmp_ud,nlatnlon,index_dv);
      }
      if(type_vd == NCL_float) {
	coerce_output_float_only(vd,tmp_vd,nlatnlon,index_dv);
      }
    }
    index_dv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dv2uvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_ud != NCL_double) NclFree(tmp_ud);
  if(type_vd != NCL_double) NclFree(tmp_vd);

  return(NhlNOERROR);
}


NhlErrorTypes dv2uvF_W( void )
{
/*
 * Input array variables
 */
  void *dv = NULL;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv, missing_rdv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing_dv;
/*
 * Output array variables
 */
  void *uvd = NULL;
  double *tmp_ud = NULL;
  double *tmp_vd = NULL;
  int ndims_uvd;
  ng_size_t *dsizes_uvd;
  NclScalar missing_uvd;
  NclBasicDataTypes type_uvd;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym, ret;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_dv, index_ud, index_vd;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *pertrb, *a, *b;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (void*)NclGetArgValue(
           0,
           1,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvF: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_dv,ndims_dv,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,
                 &missing_rdv);

/*
 * Allocate space for temporary input/output arrays. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * full arrays. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to their appropriate locations in dv.
 */
  if(type_dv != NCL_double) {
    type_uvd = NCL_float;
    tmp_ud   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vd   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_dv   = (double*)calloc(nlatnlon,sizeof(double));
    uvd      = (void*)calloc(2*total_size_in,sizeof(float));
    if(uvd == NULL || tmp_dv == NULL || tmp_ud == NULL || tmp_vd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_dv) {
      missing_uvd = missing_rdv;
    }
  }
  else {
    type_uvd = NCL_double;
    uvd      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uvd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_dv) {
      missing_uvd = missing_ddv;
    }
  } 
/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uvd  = ndims_dv + 1;
  dsizes_uvd = (ng_size_t*)calloc(ndims_uvd,sizeof(ng_size_t));  
  dsizes_uvd[0] = 2;
  for(i = 1; i <= ndims_dv; i++ ) dsizes_uvd[i] = dsizes_dv[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)+2*l1+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshaec == NULL || wvhsec == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvF: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;

/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_ud = index_dv = nmiss = 0;
  index_vd = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_dv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_dv];
    }
    if(type_uvd == NCL_double) {
/*
 * Point tmp_ud/tmp_vd to appropriate location in uvd.
 */
      tmp_ud = &((double*)uvd)[index_ud];
      tmp_vd = &((double*)uvd)[index_vd];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    if(found_missing_dv) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value.
 */
      set_subset_output_missing(uvd,index_ud,type_uvd,nlatnlon,
                                missing_ddv.doubleval);
      set_subset_output_missing(uvd,index_vd,type_uvd,nlatnlon,
                                missing_ddv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvF","shaec",&ier,&jer,&ker,&mer,6,5);
/* 
 * Reconstruct the divergent (irrotational) wind components.
 * Note the argument order idivec(...,vd,ud,...)
 */
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(didivec,DIDIVEC)(&inlat,&inlon,&isym,&one,tmp_vd,tmp_ud,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsec,&ilvhsec,
                               work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvF","vhseci,idivec",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ud,tmp_vd,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ud,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vd,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uvd == NCL_float) {
        coerce_output_float_only(uvd,tmp_ud,nlatnlon,index_ud);
        coerce_output_float_only(uvd,tmp_vd,nlatnlon,index_vd);
      }
    }
    index_ud = index_dv += nlatnlon;
    index_vd += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_dv != NCL_double) {
    NclFree(tmp_dv);
    NclFree(tmp_ud);
    NclFree(tmp_vd);
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dv2uvF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uvd,ndims_uvd,dsizes_uvd,&missing_uvd,type_uvd,0);
  }
  else {
    ret = NclReturnValue(uvd,ndims_uvd,dsizes_uvd,NULL,type_uvd,0);
  }
  NclFree(dsizes_uvd);
  return(ret);
}


NhlErrorTypes dv2uvG_W( void )
{
/*
 * Input array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_dv, missing_ddv, missing_rdv;
  NclBasicDataTypes type_dv;
  int has_missing_dv, found_missing_dv;
/*
 * Output array variables
 */
  void *uvd;
  double *tmp_ud = NULL;
  double *tmp_vd = NULL;
  int ndims_uvd;
  ng_size_t *dsizes_uvd;
  NclScalar missing_uvd;
  NclBasicDataTypes type_uvd;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym, ret;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_dv, index_ud, index_vd;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *pertrb, *a, *b;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  dv = (void*)NclGetArgValue(
           0,
           1,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_dv < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvG: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_dv,ndims_dv,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,
                 &missing_rdv);

/*
 * Allocate space for temporary input/output arrays. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * full arrays. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to their appropriate locations in dv.
 */
  if(type_dv != NCL_double) {
    type_uvd = NCL_float;
    tmp_ud   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vd   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_dv   = (double*)calloc(nlatnlon,sizeof(double));
    uvd      = (void*)calloc(2*total_size_in,sizeof(float));
    if(uvd == NULL || tmp_dv == NULL || tmp_ud == NULL || tmp_vd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_dv) {
      missing_uvd = missing_rdv;
    }
  }
  else {
    type_uvd = NCL_double;
    uvd      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uvd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_dv) {
      missing_uvd = missing_ddv;
    }
  } 
/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uvd  = ndims_dv + 1;
  dsizes_uvd = (ng_size_t*)calloc(ndims_uvd,sizeof(ng_size_t));  
  dsizes_uvd[0] = 2;
  for(i = 1; i <= ndims_dv; i++ ) dsizes_uvd[i] = dsizes_dv[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = 10*(max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon))));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon)+2*l1+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshagc == NULL || wvhsgc == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dv2uvG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_ud = index_dv = nmiss = 0;
  index_vd = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_dv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_dv];
    }
    if(type_uvd == NCL_double) {
/*
 * Point tmp_ud/tmp_vd to appropriate location in uvd.
 */
      tmp_ud = &((double*)uvd)[index_ud];
      tmp_vd = &((double*)uvd)[index_vd];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    if(found_missing_dv) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value.
 */
      set_subset_output_missing(uvd,index_ud,type_uvd,nlatnlon,
                                missing_ddv.doubleval);
      set_subset_output_missing(uvd,index_vd,type_uvd,nlatnlon,
                                missing_ddv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "dv" (divergence) 
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvG","shagc",&ier,&jer,&ker,&mer,6,5);

/* 
 * Reconstruct the divergent (irrotational) wind components.
 * note the argument order idivgc(...,vd,ud,...)
 */
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,
			       &jer);
      NGCALLF(didivgc,DIDIVGC)(&inlat,&inlon,&isym,&one,tmp_vd,tmp_ud,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsgc,&ilvhsgc,
			       work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("dv2uvG","vhsgci,idivgc",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ud,tmp_vd,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ud,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vd,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uvd == NCL_float) {
        coerce_output_float_only(uvd,tmp_ud,nlatnlon,index_ud);
        coerce_output_float_only(uvd,tmp_vd,nlatnlon,index_vd);
      }
    }
    index_ud = index_dv += nlatnlon;
    index_vd += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_dv != NCL_double) {
    NclFree(tmp_dv);
    NclFree(tmp_ud);
    NclFree(tmp_vd);
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"dv2uvG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uvd,ndims_uvd,dsizes_uvd,&missing_uvd,type_uvd,0);
  }
  else {
    ret = NclReturnValue(uvd,ndims_uvd,dsizes_uvd,NULL,type_uvd,0);
  }
  NclFree(dsizes_uvd);
  return(ret);
}


NhlErrorTypes gradsf_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
/*
 * Output array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  int has_missing_gzx, has_missing_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_z;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  gzx = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           1);
  gzy = (void*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
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
 * The output arrays must be float or double.
 */
  if((type_gzx != NCL_float && type_gzx != NCL_double) ||
     (type_gzy != NCL_float && type_gzy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: The output arrays must be float or double");
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
 * Coerce the missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,NULL);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = 10*(max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon))+nlat*(2*l1+1)));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshaec == NULL || wvhsec == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_gzx == NCL_double) {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_z];
    }
    if(type_gzy == NCL_double) {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_gzx) {
        set_subset_output_missing(gzx,index_z,type_gzx,nlatnlon,
                                  missing_dgzx.doubleval);
      }
      if(has_missing_gzy) {
        set_subset_output_missing(gzy,index_z,type_gzy,nlatnlon,
                                  missing_dgzy.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("gradsf","shaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the gradient.
 * note the argument order (...,gzy,gzx,...)
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(dgradec,DGRADEC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsec,&ilvhsec,
			       work3,&ilwork3,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("gradsf","vhseci+gradec",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_gzx,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_gzy,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_gzx == NCL_float) {
        coerce_output_float_only(gzx,tmp_gzx,nlatnlon,index_z);
      }
      if(type_gzy == NCL_float) {
        coerce_output_float_only(gzy,tmp_gzy,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"gradsf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);

  return(NhlNOERROR);
}


NhlErrorTypes gradsg_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  ng_size_t nt, nlat, nlon, nlatnlon;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
/*
 * Output array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  int has_missing_gzx, has_missing_gzy;
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_z;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  gzx = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzx, 
           dsizes_gzx,
           &missing_gzx,
           &has_missing_gzx,
           &type_gzx,
           1);
  gzy = (void*)NclGetArgValue(
           2,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
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
 * The output arrays must be float or double.
 */
  if((type_gzx != NCL_float && type_gzx != NCL_double) ||
     (type_gzy != NCL_float && type_gzy != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: The output arrays must be float or double");
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
 * Coerce the missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,NULL);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3  = 10*(max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon))+nlat*(2*l1+1)));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 10*(4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15);

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshagc == NULL || wvhsgc == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"gradsg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_gzx == NCL_double) {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_z];
    }
    if(type_gzy == NCL_double) {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_gzx) {
        set_subset_output_missing(gzx,index_z,type_gzx,nlatnlon,
                                  missing_dgzx.doubleval);
      }
      if(has_missing_gzy) {
        set_subset_output_missing(gzy,index_z,type_gzy,nlatnlon,
                                  missing_dgzy.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "z" (a scalar divergence)
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
			     a,b,&imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,
			     &ker);

      NGCALLF(dchkerr,DCHKERR)("gradsg","shagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the gradient.
 * Note the argument order (...,gzy,gzx,...)
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,
			       &jer);
      NGCALLF(dgradgc,DGRADGC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,
			       &iidvw,&ijdvw,a,b,&imdab,&indab,wvhsgc,&ilvhsgc,
			       work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("gradsg","vhsgci+gradgc",&ier,&jer,&ker,&mer,
			       6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_gzx,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_gzy,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_gzx == NCL_float) {
        coerce_output_float_only(gzx,tmp_gzx,nlatnlon,index_z);
      }
      if(type_gzy == NCL_float) {
        coerce_output_float_only(gzy,tmp_gzy,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"gradsg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);

  return(NhlNOERROR);
}

NhlErrorTypes igradsf_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  int has_missing_gzx, has_missing_gzy, found_missing_gzx, found_missing_gzy;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_gzxy;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  gzy = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           DONT_CARE);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           1);
/*
 * The output array must be float or double.
 */
  if(type_z != NCL_float && type_z != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce missing values.
 */
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,NULL);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in gzx, gzy, and z.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  l3     = max(nlat,(nlon+2)/2);
  lwork1 = nlatnlon;
  lwork2  = 10*(max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon))));
  lwork3  = 10*(max(nlat+1,nlat*(nlon+max(3*l2,nlon)+2*l3+1)));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec = 10*(4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15);
  lshsec = 10*(2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15);

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
    if((nlon > INT_MAX) ||
       (nlat > INT_MAX) ||
       (nt > INT_MAX) ||
       (lvhaec > INT_MAX) ||
       (lshsec > INT_MAX) ||
       (idvw > INT_MAX) ||
       (jdvw > INT_MAX) ||
       (mdab > INT_MAX) ||
       (ndab > INT_MAX) ||
       (ldwork1 > INT_MAX) ||
       (ldwork2 > INT_MAX) ||
       (lwork2 > INT_MAX) ||
       (lwork3 > INT_MAX)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsf: one or more input dimension sizes is greater than INT_MAX");
      return(NhlFATAL);
    }
    inlon = (int) nlon;
    inlat = (int) nlat;
    ilvhaec = (int) lvhaec;
    ilshsec = (int) lshsec;
    iidvw = (int) idvw;
    ijdvw = (int) jdvw;
    imdab = (int) mdab;
    indab = (int) ndab;
    ildwork1 = (int) ldwork1;
    ildwork2 = (int) ldwork2;
    ilwork2 = (int) lwork2;
    ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you cannot check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_gzxy = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_gzx != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzx (tmp_gzx) to double.
 */
      coerce_subset_input_double(gzx,tmp_gzx,index_gzxy,type_gzx,nlatnlon,0,
                                 &missing_gzx,&missing_dgzx);
    }
    else {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_gzxy];
    }
    if(type_gzy != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzy (tmp_gzy) to double.
 */
      coerce_subset_input_double(gzy,tmp_gzy,index_gzxy,type_gzy,nlatnlon,0,
                                 &missing_gzy,&missing_dgzy);
    }
    else {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_gzxy];
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_gzxy];
    }
/*
 * Check for missing values.
 */
    found_missing_gzx = contains_missing(tmp_gzx,nlatnlon,has_missing_gzx,
                                         missing_dgzx.doubleval);
    found_missing_gzy = contains_missing(tmp_gzy,nlatnlon,has_missing_gzy,
                                         missing_dgzy.doubleval);

    if(found_missing_gzx || found_missing_gzy) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_z) {
        set_subset_output_missing(z,index_gzxy,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_gzx,tmp_gzy,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,
			     &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
			     wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("igradsf","vhaec",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute the scalar function given the input vector
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(digradec,DIGRADEC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
				 br,bi,&imdab,&indab,wshsec,&ilshsec,
				 work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("igradsf","shseci+igradec",&ier,&jer,&ker,
			       &mer,7,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_gzxy);
      }
    }
    index_gzxy += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);
  if(type_z != NCL_double) NclFree(tmp_z);

  return(NhlNOERROR);
}


NhlErrorTypes igradsF_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  NclScalar missing_gzx, missing_gzy;
  NclScalar missing_dgzx, missing_rgzx, missing_dgzy, missing_rgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  int has_missing_gzx, has_missing_gzy, found_missing_gzx, found_missing_gzy;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_gzxy;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  gzy = (void*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce missing values.
 */
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,
                 &missing_rgzx);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,
                 &missing_rgzy);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in gzx, gzy, and z.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_gzx != NCL_double && type_gzy != NCL_double) {
    type_z = NCL_float;
    tmp_z  = (double*)calloc(nlatnlon,sizeof(double));
    z = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_z = NCL_double;
    z = (void*)calloc(total_size_in,sizeof(double));
  }
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_gzx) {
    if(type_z == NCL_double) missing_z = missing_dgzx;
    else                     missing_z = missing_rgzx;
    missing_dz = missing_dgzx;
  }
  else if(has_missing_gzy) {
    if(type_z == NCL_double) missing_z = missing_dgzy;
    else                     missing_z = missing_rgzy;
    missing_dz = missing_dgzy;
  }
/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+2)/2);
  lwork1  = nlatnlon;
  lwork2  = 10*(max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon))));
  lwork3  = 10*(max(nlat+1,nlat*(nlon+max(3*l2,nlon)+2*l3+1)));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 10*(4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15);
  lshsec  = 10*(2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15);

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_gzxy = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_gzx != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzx (tmp_gzx) to double.
 */
      coerce_subset_input_double(gzx,tmp_gzx,index_gzxy,type_gzx,nlatnlon,0,
                                 &missing_gzx,&missing_dgzx);
    }
    else {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_gzxy];
    }
    if(type_gzy != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzy (tmp_gzy) to double.
 */
      coerce_subset_input_double(gzy,tmp_gzy,index_gzxy,type_gzy,nlatnlon,0,
                                 &missing_gzy,&missing_dgzy);
    }
    else {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_gzxy];
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_gzxy];
    }
/*
 * Check for missing values.
 */
    found_missing_gzx = contains_missing(tmp_gzx,nlatnlon,has_missing_gzx,
                                         missing_dgzx.doubleval);
    found_missing_gzy = contains_missing(tmp_gzy,nlatnlon,has_missing_gzy,
                                         missing_dgzy.doubleval);

    if(found_missing_gzx || found_missing_gzy) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(z,index_gzxy,type_z,nlatnlon,
                                missing_dz.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_gzx,tmp_gzy,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,
			     &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
			     wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("igradsF","vhaec",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute the scalar function given the input vector
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(digradec,DIGRADEC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
				 br,bi,&imdab,&indab,wshsec,&ilshsec,
				 work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("igradsF","shseci+igradec",&ier,&jer,&ker,
			       &mer,7,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_gzxy);
      }
    }
    index_gzxy += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);
  if(type_z != NCL_double) NclFree(tmp_z);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(z,ndims_gzx,dsizes_gzx,&missing_z,type_z,0));
  }
  else {
    return(NclReturnValue(z,ndims_gzx,dsizes_gzx,NULL,type_z,0));
  }
}



NhlErrorTypes igradsg_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  NclScalar missing_gzx, missing_gzy, missing_dgzx, missing_dgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  int has_missing_gzx, has_missing_gzy, found_missing_gzx, found_missing_gzy;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_gzxy;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  gzy = (void*)NclGetArgValue(
           1,
           3,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           DONT_CARE);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           1);
/*
 * The output array must be float or double.
 */
  if(type_z != NCL_float && type_z != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The output array must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce missing values.
 */
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,NULL);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in gzx, gzy, and z.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+2)/2);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l3+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_gzxy = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_gzx != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzx (tmp_gzx) to double.
 */
      coerce_subset_input_double(gzx,tmp_gzx,index_gzxy,type_gzx,nlatnlon,0,
                                 &missing_gzx,&missing_dgzx);
    }
    else {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_gzxy];
    }
    if(type_gzy != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzy (tmp_gzy) to double.
 */
      coerce_subset_input_double(gzy,tmp_gzy,index_gzxy,type_gzy,nlatnlon,0,
                                 &missing_gzy,&missing_dgzy);
    }
    else {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_gzxy];
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_gzxy];
    }
/*
 * Check for missing values.
 */
    found_missing_gzx = contains_missing(tmp_gzx,nlatnlon,has_missing_gzx,
                                         missing_dgzx.doubleval);
    found_missing_gzy = contains_missing(tmp_gzy,nlatnlon,has_missing_gzy,
                                         missing_dgzy.doubleval);

    if(found_missing_gzx || found_missing_gzy) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_z) {
        set_subset_output_missing(z,index_gzxy,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_gzx,tmp_gzy,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,
			     &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
			     wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("igradsg","vhagc",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute the scalar function given the input vector
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2, &jer);

      NGCALLF(digradgc,DIGRADGC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
				 br,bi,&imdab,&indab,wshsgc,&ilshsgc,
				 work3,&ilwork3,&ker);
                             
      NGCALLF(dchkerr,DCHKERR)("igradsg","shsgci+igradgc",&ier,&jer,&ker,
			       &mer,7,14);

/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_gzxy);
      }
    }
    index_gzxy += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);
  if(type_z != NCL_double) NclFree(tmp_z);

  return(NhlNOERROR);
}


NhlErrorTypes igradsG_W( void )
{
/*
 * Input array variables
 */
  void *gzx, *gzy;
  double *tmp_gzx = NULL;
  double *tmp_gzy = NULL;
  int ndims_gzx;
  ng_size_t dsizes_gzx[NCL_MAX_DIMENSIONS];
  int ndims_gzy;
  ng_size_t dsizes_gzy[NCL_MAX_DIMENSIONS];
  NclScalar missing_gzx, missing_gzy;
  NclScalar missing_dgzx, missing_rgzx, missing_dgzy, missing_rgzy;
  NclBasicDataTypes type_gzx, type_gzy;
  int has_missing_gzx, has_missing_gzy, found_missing_gzx, found_missing_gzy;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_gzxy;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  gzy = (void*)NclGetArgValue(
           1,
           2,
           &ndims_gzy, 
           dsizes_gzy,
           &missing_gzy,
           &has_missing_gzy,
           &type_gzy,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_gzx != ndims_gzy || ndims_gzx < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce missing values.
 */
  coerce_missing(type_gzx,has_missing_gzx,&missing_gzx,&missing_dgzx,
                 &missing_rgzx);
  coerce_missing(type_gzy,has_missing_gzy,&missing_gzy,&missing_dgzy,
                 &missing_rgzy);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in gzx, gzy, and z.
 */
  if(type_gzx != NCL_double) {
    tmp_gzx = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzx == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for coercing gzx array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_gzy != NCL_double) {
    tmp_gzy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_gzy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for coercing gzy array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_gzx != NCL_double && type_gzy != NCL_double) {
    type_z = NCL_float;
    tmp_z  = (double*)calloc(nlatnlon,sizeof(double));
    z = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_z = NCL_double;
    z = (void*)calloc(total_size_in,sizeof(double));
  }
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_gzx) {
    if(type_z == NCL_double) missing_z = missing_dgzx;
    else                     missing_z = missing_rgzx;
    missing_dz = missing_dgzx;
  }
  else if(has_missing_gzy) {
    if(type_z == NCL_double) missing_z = missing_dgzy;
    else                     missing_z = missing_rgzy;
    missing_dz = missing_dgzy;
  }
/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+2)/2);
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l3+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"igradsG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_gzxy = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_gzx != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzx (tmp_gzx) to double.
 */
      coerce_subset_input_double(gzx,tmp_gzx,index_gzxy,type_gzx,nlatnlon,0,
                                 &missing_gzx,&missing_dgzx);
    }
    else {
/*
 * Point tmp_gzx to appropriate location in gzx.
 */
      tmp_gzx = &((double*)gzx)[index_gzxy];
    }
    if(type_gzy != NCL_double) {
/*
 * Coerce nlat x nlon subsection of gzy (tmp_gzy) to double.
 */
      coerce_subset_input_double(gzy,tmp_gzy,index_gzxy,type_gzy,nlatnlon,0,
                                 &missing_gzy,&missing_dgzy);
    }
    else {
/*
 * Point tmp_gzy to appropriate location in gzy.
 */
      tmp_gzy = &((double*)gzy)[index_gzxy];
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_gzxy];
    }
/*
 * Check for missing values.
 */
    found_missing_gzx = contains_missing(tmp_gzx,nlatnlon,has_missing_gzx,
                                         missing_dgzx.doubleval);
    found_missing_gzy = contains_missing(tmp_gzy,nlatnlon,has_missing_gzy,
                                         missing_dgzy.doubleval);

    if(found_missing_gzx || found_missing_gzy) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(z,index_gzxy,type_z,nlatnlon,
                                missing_dz.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_gzx,tmp_gzy,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_gzy,tmp_gzx,&iidvw,
			     &ijdvw,br,bi,cr,ci,&imdab,&indab,wvhagc,&ilvhagc,
			     work2,&ilwork2,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("igradsG","vhagc",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute the scalar function given the input vector
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(digradgc,DIGRADGC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,
				 br,bi,&imdab,&indab,wshsgc,&ilshsgc,work3,
				 &ilwork3,&ker);
                             
      NGCALLF(dchkerr,DCHKERR)("igradsG","shsgci+igradgc",&ier,&jer,&ker,
			       &mer,7,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_gzx,tmp_gzy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_gzxy);
      }
    }
    index_gzxy += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);
  if(type_gzx != NCL_double) NclFree(tmp_gzx);
  if(type_gzy != NCL_double) NclFree(tmp_gzy);
  if(type_z != NCL_double) NclFree(tmp_z);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"igradsG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(z,ndims_gzx,dsizes_gzx,&missing_z,type_z,0));
  }
  else {
    return(NclReturnValue(z,ndims_gzx,dsizes_gzx,NULL,type_z,0));
  }
}

NhlErrorTypes ilapsf_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *tmp_zlap = NULL;
  double *tmp_zlmbda = NULL;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
  int ndims_zlmbda;
  ng_size_t dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  int has_missing_zlap, has_missing_zlmbda;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  NclScalar missing_z, missing_dz;
  int has_missing_z;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  ng_size_t total_size_in, scalar_zlmbda;
  ng_size_t nt, nlat, nlon, nlatnlon;
  int found_missing_zlap, found_missing_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_zlap;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshaec, lshsec;
  double *work1, *work2, *work3, *wshaec, *wshsec, *pertrb, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  zlmbda = (void*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           DONT_CARE);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
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
 * Check the input array zlmbda.  If it is not a constant, then it must
 * be equal in size to the rightmost-2 dimensions of zlap.
 */
  scalar_zlmbda = is_scalar(ndims_zlmbda,dsizes_zlmbda);
  if(!scalar_zlmbda) {
    if(ndims_zlmbda != (ndims_zlap-2)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,
                 &missing_dzlap,NULL);
  coerce_missing(type_zlmbda,has_missing_zlmbda,&missing_zlmbda,
                 &missing_dzlmbda,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);

/*
 * Allocate space for temporary input and output. The temporary array
 * tmp_zlap is just big enough to hold a 2-dimensional subsection of the
 * zlap array. The temporary array tmp_zlmbda is just big enough to hold
 * one double value. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to the appropriate locations in zlap and zlmbda.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_zlmbda != NCL_double) {
    tmp_zlmbda = (double*)calloc(1,sizeof(double));
    if(tmp_zlmbda == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for coercing zlmbda array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output array, if not already double.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));
  a      =  (double*)calloc(mdab*ndab,sizeof(double));
  b      =  (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshaec == NULL || wshsec == NULL || pertrb == NULL || 
      a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_zlap = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_zlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of zlap (tmp_zlap) to double.
 */
      coerce_subset_input_double(zlap,tmp_zlap,index_zlap,type_zlap,
                                 nlatnlon,0,&missing_zlap,&missing_dzlap);
    }
    else {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_zlap];
    }
/*
 * If zlmbda is a scalar, only coerce it the first time through the 
 * loop. Otherwise, coerce one element of zlmbda each time through
 * the loop.
 */
    if(!scalar_zlmbda || (scalar_zlmbda && i==0)) {
      if(type_zlmbda != NCL_double) {
/*
 * Coerce one element of zlmbda (tmp_zlmbda) to double.
 */
        coerce_subset_input_double(zlmbda,tmp_zlmbda,i,type_zlmbda,
                                   1,0,&missing_zlmbda,&missing_dzlmbda);
      }
      else {
/*
 * Point tmp_zlmbda to appropriate location in zlmbda.
 */
        tmp_zlmbda = &((double*)zlmbda)[i];
      }
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_zlap];
    }
/*
 * Check for missing values.
 */
    found_missing_zlap = contains_missing(tmp_zlap,nlatnlon,has_missing_zlap,
                                          missing_dzlap.doubleval);
    found_missing_zlmbda = contains_missing(tmp_zlmbda,1,has_missing_zlmbda,
                                            missing_dzlmbda.doubleval);
    if(found_missing_zlap || found_missing_zlmbda) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_z) {
        set_subset_output_missing(z,index_zlap,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_zlap,work1);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,a,b,
			     &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("ilapsf","shaec",&ier,&jer,&ker,&mer,6,5);
/* 
 * Invert the laplacian
 */ 
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,
			       &jer);
      NGCALLF(dislapec,DISLAPEC)(&inlat,&inlon,&isym,&one,tmp_zlmbda,tmp_z,
				 &iidvw,&ijdvw,a,b,&imdab,&indab,wshsec,&ilshsec,
				 work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsf","shseci+islapec",&ier,&jer,&ker,&mer,
			       6,14);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_zlap);
      }
    }
    index_zlap += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshaec);
  NclFree(wshsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_zlap   != NCL_double) NclFree(tmp_zlap);
  if(type_zlmbda != NCL_double) NclFree(tmp_zlmbda);
  if(type_z      != NCL_double) NclFree(tmp_z);

  return(NhlNOERROR);
}


NhlErrorTypes ilapsF_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *tmp_zlap = NULL;
  double *tmp_zlmbda = NULL;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
  int ndims_zlmbda;
  ng_size_t dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclScalar missing_rzlap;
  NclBasicDataTypes type_zlap, type_zlmbda;
  int has_missing_zlap, has_missing_zlmbda;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  ng_size_t total_size_in;
  int scalar_zlmbda;
  ng_size_t nt, nlat, nlon, nlatnlon;
  int found_missing_zlap, found_missing_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_zlap;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshaec, lshsec;
  double *work1, *work2, *work3, *wshaec, *wshsec, *pertrb, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  zlmbda = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           DONT_CARE);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is not a constant, then it must
 * be equal in size to the rightmost-2 dimensions of zlap.
 */
  scalar_zlmbda = is_scalar(ndims_zlmbda,dsizes_zlmbda);
  if(!scalar_zlmbda) {
    if(ndims_zlmbda != (ndims_zlap-2)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,
                 &missing_dzlap,&missing_rzlap);
  coerce_missing(type_zlmbda,has_missing_zlmbda,&missing_zlmbda,
                 &missing_dzlmbda,NULL);
/*
 * Allocate space for temporary input and output. The temporary array
 * tmp_zlap is just big enough to hold a 2-dimensional subsection of the
 * zlap array. The temporary array tmp_zlmbda is just big enough to hold
 * one double value. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to the appropriate locations in zlap and zlmbda.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_zlmbda != NCL_double) {
    tmp_zlmbda = (double*)calloc(1,sizeof(double));
    if(tmp_zlmbda == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for coercing zlmbda array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array
 */
  if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
    type_z = NCL_float;
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    z = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_z = NCL_double;
    z = (void*)calloc(total_size_in,sizeof(double));
  }
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Set a missing value for the output if zlap has a missing value.
 */
  if(has_missing_zlap) {
    if(type_z == NCL_double) missing_z = missing_dzlap;
    else                     missing_z = missing_rzlap;
    missing_dz = missing_dzlap;
  }

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  /*  lwork3  = max(nlat+1,nlat*(2*nlon+max(6*l2,nlon)+2*l3+1)); */
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshaec == NULL || wshsec == NULL || pertrb == NULL || 
      a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_zlap = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_zlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of zlap (tmp_zlap) to double.
 */
      coerce_subset_input_double(zlap,tmp_zlap,index_zlap,type_zlap,
                                 nlatnlon,0,&missing_zlap,&missing_dzlap);
    }
    else {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_zlap];
    }
/*
 * If zlmbda is a scalar, only coerce it the first time through the 
 * loop. Otherwise, coerce one element of zlmbda each time through
 * the loop.
 */
    if(!scalar_zlmbda || (scalar_zlmbda && i==0)) {
      if(type_zlmbda != NCL_double) {
/*
 * Coerce one element of zlmbda (tmp_zlmbda) to double.
 */
        coerce_subset_input_double(zlmbda,tmp_zlmbda,i,type_zlmbda,
                                   1,0,&missing_zlmbda,&missing_dzlmbda);
      }
      else {
/*
 * Point tmp_zlmbda to appropriate location in zlmbda.
 */
        tmp_zlmbda = &((double*)zlmbda)[i];
      }
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_zlap];
    }
/*
 * Check for missing values.
 */
    found_missing_zlap = contains_missing(tmp_zlap,nlatnlon,has_missing_zlap,
                                          missing_dzlap.doubleval);
    found_missing_zlmbda = contains_missing(tmp_zlmbda,1,has_missing_zlmbda,
                                            missing_dzlmbda.doubleval);
    if(found_missing_zlap || found_missing_zlmbda) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_zlap) {
        set_subset_output_missing(z,index_zlap,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_zlap,work1);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,a,b,
			     &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsF","shaec",&ier,&jer,&ker,&mer,6,5);
/* 
 * Invert the laplacian
 */ 
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,
			       &jer);
      NGCALLF(dislapec,DISLAPEC)(&inlat,&inlon,&isym,&one,tmp_zlmbda,tmp_z,
				 &iidvw,&ijdvw,a,b,&imdab,&indab,wshsec,&ilshsec,
				 work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsF","shseci+islapec",&ier,&jer,&ker,&mer,
			       6,14);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_zlap);
      }
    }
    index_zlap += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshaec);
  NclFree(wshsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_zlap   != NCL_double) NclFree(tmp_zlap);
  if(type_zlmbda != NCL_double) NclFree(tmp_zlmbda);
  if(type_z      != NCL_double) NclFree(tmp_z);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(z,ndims_zlap,dsizes_zlap,&missing_z,type_z,0));
  }
  else {
    return(NclReturnValue(z,ndims_zlap,dsizes_zlap,NULL,type_z,0));
  }
}


NhlErrorTypes ilapsg_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *tmp_zlap = NULL;
  double *tmp_zlmbda = NULL;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
  int ndims_zlmbda;
  ng_size_t dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclBasicDataTypes type_zlap, type_zlmbda;
  int has_missing_zlap, has_missing_zlmbda;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  NclScalar missing_z, missing_dz;
  int has_missing_z;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  ng_size_t total_size_in, scalar_zlmbda;
  ng_size_t nt, nlat, nlon, nlatnlon;
  int found_missing_zlap, found_missing_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_zlap;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshagc, lshsgc;
  double *work1, *work2, *work3, *wshagc, *wshsgc, *pertrb, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  zlmbda = (void*)NclGetArgValue(
           1,
           3,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           DONT_CARE);
/*
 * Get output array.
 */
  z = (void*)NclGetArgValue(
           2,
           3,
           &ndims_z, 
           dsizes_z,
           &missing_z,
           &has_missing_z,
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
 * Check the input array zlmbda.  If it is not a constant, then it must
 * be equal in size to the rightmost-2 dimensions of zlap.
 */
  scalar_zlmbda = is_scalar(ndims_zlmbda,dsizes_zlmbda);
  if(!scalar_zlmbda) {
    if(ndims_zlmbda != (ndims_zlap-2)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,
                 &missing_dzlap,NULL);
  coerce_missing(type_zlmbda,has_missing_zlmbda,&missing_zlmbda,
                 &missing_dzlmbda,NULL);
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);

/*
 * Allocate space for temporary input and output. The temporary array
 * tmp_zlap is just big enough to hold a 2-dimensional subsection of the
 * zlap array. The temporary array tmp_zlmbda is just big enough to hold
 * one double value. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to the appropriate locations in zlap and zlmbda.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_zlmbda != NCL_double) {
    tmp_zlmbda = (double*)calloc(1,sizeof(double));
    if(tmp_zlmbda == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for coercing zlmbda array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output array, if not already double.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshagc == NULL || wshsgc == NULL || pertrb == NULL || 
      a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_zlap = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_zlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of zlap (tmp_zlap) to double.
 */
      coerce_subset_input_double(zlap,tmp_zlap,index_zlap,type_zlap,
                                 nlatnlon,0,&missing_zlap,&missing_dzlap);
    }
    else {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_zlap];
    }
/*
 * If zlmbda is a scalar, only coerce it the first time through the 
 * loop. Otherwise, coerce one element of zlmbda each time through
 * the loop.
 */
    if(!scalar_zlmbda || (scalar_zlmbda && i==0)) {
      if(type_zlmbda != NCL_double) {
/*
 * Coerce one element of zlmbda (tmp_zlmbda) to double.
 */
        coerce_subset_input_double(zlmbda,tmp_zlmbda,i,type_zlmbda,
                                   1,0,&missing_zlmbda,&missing_dzlmbda);
      }
      else {
/*
 * Point tmp_zlmbda to appropriate location in zlmbda.
 */
        tmp_zlmbda = &((double*)zlmbda)[i];
      }
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_zlap];
    }
/*
 * Check for missing values.
 */
    found_missing_zlap = contains_missing(tmp_zlap,nlatnlon,has_missing_zlap,
                                          missing_dzlap.doubleval);
    found_missing_zlmbda = contains_missing(tmp_zlmbda,1,has_missing_zlmbda,
                                            missing_dzlmbda.doubleval);
    if(found_missing_zlap || found_missing_zlmbda) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_z) {
        set_subset_output_missing(z,index_zlap,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_zlap,work1);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,
			       &jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,a,b,
			     &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("ilapsg","shagc",&ier,&jer,&ker,&mer,6,5);
/* 
 * Invert the laplacian
 */ 
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
      NGCALLF(dislapgc,DISLAPGC)(&inlat,&inlon,&isym,&one,tmp_zlmbda,tmp_z,
				 &iidvw,&ijdvw,a,b,&imdab,&indab,wshsgc,&ilshsgc,
				 work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsg","shsgci+islapgc",&ier,&jer,&ker,&mer,
                                 6,14);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_zlap);
      }
    }
    index_zlap += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshagc);
  NclFree(wshsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_zlap   != NCL_double) NclFree(tmp_zlap);
  if(type_zlmbda != NCL_double) NclFree(tmp_zlmbda);
  if(type_z      != NCL_double) NclFree(tmp_z);

  return(NhlNOERROR);
}


NhlErrorTypes ilapsG_W( void )
{
/*
 * Input array variables
 */
  void *zlap, *zlmbda;
  double *tmp_zlap = NULL;
  double *tmp_zlmbda = NULL;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
  int ndims_zlmbda;
  ng_size_t dsizes_zlmbda[NCL_MAX_DIMENSIONS];
  NclScalar missing_zlap, missing_zlmbda, missing_dzlap, missing_dzlmbda;
  NclScalar missing_rzlap;
  NclBasicDataTypes type_zlap, type_zlmbda;
  int has_missing_zlap, has_missing_zlmbda;
/*
 * Output array variables
 */
  void *z;
  double *tmp_z = NULL;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  ng_size_t total_size_in, scalar_zlmbda;
  ng_size_t nt, nlat, nlon, nlatnlon;
  int found_missing_zlap, found_missing_zlmbda;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_zlap;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshagc, lshsgc;
  double *work1, *work2, *work3, *wshagc, *wshsgc, *pertrb, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  zlmbda = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlmbda, 
           dsizes_zlmbda,
           &missing_zlmbda,
           &has_missing_zlmbda,
           &type_zlmbda,
           DONT_CARE);
/*
 * The grids coming in must be 1 and 3-dimensional.
 */
  if( ndims_zlap < 2 || ndims_zlmbda < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: The first input array must be at least 2-dimensional and the second input array must be at least 1-dimensional");
    return(NhlFATAL);
  }
/*
 * Check the input array zlmbda.  If it is not a constant, then it must
 * be equal in size to the rightmost-2 dimensions of zlap.
 */
  scalar_zlmbda = is_scalar(ndims_zlmbda,dsizes_zlmbda);
  if(!scalar_zlmbda) {
    if(ndims_zlmbda != (ndims_zlap-2)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
      return(NhlFATAL);
    }
    for( i = 0; i < ndims_zlmbda; i++ ) {
      if( dsizes_zlmbda[i] != dsizes_zlap[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: zlmbda must either be a scalar or have the same dimensions as all but the last two dimensions of the first input array");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_zlap,ndims_zlap,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,
                 &missing_dzlap,&missing_rzlap);
  coerce_missing(type_zlmbda,has_missing_zlmbda,&missing_zlmbda,
                 &missing_dzlmbda,NULL);
/*
 * Allocate space for temporary input and output. The temporary array
 * tmp_zlap is just big enough to hold a 2-dimensional subsection of the
 * zlap array. The temporary array tmp_zlmbda is just big enough to hold
 * one double value. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to the appropriate locations in zlap and zlmbda.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_zlmbda != NCL_double) {
    tmp_zlmbda = (double*)calloc(1,sizeof(double));
    if(tmp_zlmbda == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for coercing zlmbda array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array
 */
  if(type_zlap != NCL_double && type_zlmbda != NCL_double) {
    type_z = NCL_float;
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    z = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_z == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_z = NCL_double;
    z = (void*)calloc(total_size_in,sizeof(double));
  }
  if( z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Set a missing value for the output if zlap has a missing value.
 */
  if(has_missing_zlap) {
    if(type_z == NCL_double) missing_z = missing_dzlap;
    else                     missing_z = missing_rzlap;
    missing_dz = missing_dzlap;
  }

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat*(nlat+4);
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshagc == NULL || wshsgc == NULL || pertrb == NULL || 
      a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapsG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_zlap = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_zlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of zlap (tmp_zlap) to double.
 */
      coerce_subset_input_double(zlap,tmp_zlap,index_zlap,type_zlap,
                                 nlatnlon,0,&missing_zlap,&missing_dzlap);
    }
    else {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_zlap];
    }
/*
 * If zlmbda is a scalar, only coerce it the first time through the 
 * loop. Otherwise, coerce one element of zlmbda each time through
 * the loop.
 */
    if(!scalar_zlmbda || (scalar_zlmbda && i==0)) {
      if(type_zlmbda != NCL_double) {
/*
 * Coerce one element of zlmbda (tmp_zlmbda) to double.
 */
        coerce_subset_input_double(zlmbda,tmp_zlmbda,i,type_zlmbda,
                                   1,0,&missing_zlmbda,&missing_dzlmbda);
      }
      else {
/*
 * Point tmp_zlmbda to appropriate location in zlmbda.
 */
        tmp_zlmbda = &((double*)zlmbda)[i];
      }
    }
    if(type_z == NCL_double) {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_zlap];
    }
/*
 * Check for missing values.
 */
    found_missing_zlap = contains_missing(tmp_zlap,nlatnlon,has_missing_zlap,
                                          missing_dzlap.doubleval);
    found_missing_zlmbda = contains_missing(tmp_zlmbda,1,has_missing_zlmbda,
                                            missing_dzlmbda.doubleval);
    if(found_missing_zlap || found_missing_zlmbda) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_zlap) {
        set_subset_output_missing(z,index_zlap,type_z,nlatnlon,
                                  missing_dz.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_zlap,work1);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,
			       &jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,a,b,
			     &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsG","shagc",&ier,&jer,&ker,&mer,6,5);
/* 
 * Invert the laplacian
 */ 
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
      NGCALLF(dislapgc,DISLAPGC)(&inlat,&inlon,&isym,&one,tmp_zlmbda,tmp_z,
				 &iidvw,&ijdvw,a,b,&imdab,&indab,wshsgc,&ilshsgc,
				 work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapsG","shsgci+islapgc",&ier,&jer,&ker,&mer,
                                 6,14);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_z,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_z == NCL_float) {
        coerce_output_float_only(z,tmp_z,nlatnlon,index_zlap);
      }
    }
    index_zlap += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshagc);
  NclFree(wshsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_zlap   != NCL_double) NclFree(tmp_zlap);
  if(type_zlmbda != NCL_double) NclFree(tmp_zlmbda);
  if(type_z      != NCL_double) NclFree(tmp_z);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapsG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(z,ndims_zlap,dsizes_zlap,&missing_z,type_z,0));
  }
  else {
    return(NclReturnValue(z,ndims_zlap,dsizes_zlap,NULL,type_z,0));
  }
}


NhlErrorTypes ilapvf_W( void )
{
/*
 * Input array variables
 */
  void *ulap, *vlap;
  double *tmp_ulap = NULL;
  double *tmp_vlap = NULL;
  int ndims_ulap;
  ng_size_t dsizes_ulap[NCL_MAX_DIMENSIONS];
  int ndims_vlap;
  ng_size_t dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap, missing_dulap, missing_dvlap;
  NclBasicDataTypes type_ulap, type_vlap;
  int has_missing_ulap, has_missing_vlap;
  int found_missing_ulap, found_missing_vlap;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_du;
  NclScalar missing_v, missing_dv;
  int has_missing_u, has_missing_v;
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  ng_size_t total_size_in, nt, nlat, nlon, nlatnlon;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lvhaec, lvhsec;
  double *work1, *work2, *work3, *wvhaec, *wvhsec, *br, *bi, *cr, *ci, *dwork;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  vlap = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_ulap,has_missing_ulap,&missing_ulap,&missing_dulap,
                 NULL);
  coerce_missing(type_vlap,has_missing_vlap,&missing_vlap,&missing_dvlap,
                 NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input. The temporary arrays tmp_ulap
 * and tmp_vlap are just big enough to hold a 2-dimensional
 * subsection of the ulap, vlap array. We only need to allocate space
 * for them if the input is not already double. Otherwise, we just 
 * have them point to the appropriate locations in ulap and vlap.
 */
  if(type_ulap != NCL_double) {
    tmp_ulap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ulap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for coercing ulap array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vlap != NCL_double) {
    tmp_vlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for coercing vlap array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = min(nlat,nlon/2);
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = nlat*(2*nlon+max(6*l2,nlon))+nlat*(4*l3+1);
  ldwork  = 2*(nlat+2);
  lvhaec  = 4*nlat*l2+3*max(l3-2,0)*(2*nlat-l3-1)+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wvhaec == NULL || wvhsec == NULL || br == NULL || bi == NULL ||
      cr == NULL || ci == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_ulap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of ulap (tmp_ulap) to double.
 */
      coerce_subset_input_double(ulap,tmp_ulap,index_uv,type_ulap,
                                 nlatnlon,0,&missing_ulap,&missing_dulap);
    }
    else {
/*
 * Point tmp_ulap to appropriate location in ulap.
 */
      tmp_ulap = &((double*)ulap)[index_uv];
    }

    if(type_vlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vlap (tmp_vlap) to double.
 */
      coerce_subset_input_double(vlap,tmp_vlap,index_uv,type_vlap,
                                 nlatnlon,0,&missing_vlap,&missing_dvlap);
    }
    else {
/*
 * Point tmp_vlap to appropriate location in vlap.
 */
      tmp_vlap = &((double*)vlap)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_ulap = contains_missing(tmp_ulap,nlatnlon,has_missing_ulap,
                                          missing_dulap.doubleval);
    found_missing_vlap = contains_missing(tmp_vlap,nlatnlon,has_missing_vlap,
                                          missing_dvlap.doubleval);
    if(found_missing_ulap || found_missing_vlap) {
      nmiss++;
/*
 * Set all elements of the 2D grids to missing values, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_dv.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_ulap,tmp_vlap,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork,&ildwork,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_vlap,tmp_ulap,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapvf","vhaec",&ier,&jer,&ker,&mer,6,5);
/* 
 * Compute the vector laplacian using the vector spherical harmonic 
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork,&ildwork,
			       &jer);
      NGCALLF(divlapec,DIVLAPEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                                   &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                   wvhsec,&ilvhsec,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapvf","vhseci,ivlapec",&ier,&jer,&ker,&mer,
                                 6,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ulap,tmp_vlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scalesqrd,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wvhaec);
  NclFree(wvhsec);
  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);

  if(type_ulap != NCL_double) NclFree(tmp_ulap);
  if(type_vlap != NCL_double) NclFree(tmp_vlap);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes ilapvg_W( void )
{
/*
 * Input array variables
 */
  void *ulap, *vlap;
  double *tmp_ulap = NULL;
  double *tmp_vlap = NULL;
  int ndims_ulap;
  ng_size_t dsizes_ulap[NCL_MAX_DIMENSIONS];
  int ndims_vlap;
  ng_size_t dsizes_vlap[NCL_MAX_DIMENSIONS];
  NclScalar missing_ulap, missing_vlap, missing_dulap, missing_dvlap;
  NclBasicDataTypes type_ulap, type_vlap;
  int has_missing_ulap, has_missing_vlap;
  int found_missing_ulap, found_missing_vlap;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_du;
  NclScalar missing_v, missing_dv;
  int has_missing_u, has_missing_v;
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  ng_size_t total_size_in, nt, nlat, nlon, nlatnlon;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double scalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lvhagc, lvhsgc;
  double *work1, *work2, *work3, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci, *dwork;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  vlap = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and the same # of
 * dimensions.
 */
  if( ndims_ulap != ndims_vlap || ndims_ulap < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_ulap,has_missing_ulap,&missing_ulap,&missing_dulap,
                 NULL);
  coerce_missing(type_vlap,has_missing_vlap,&missing_vlap,&missing_dvlap,
                 NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input. The temporary arrays tmp_ulap
 * and tmp_vlap are just big enough to hold a 2-dimensional
 * subsection of the ulap, vlap array. We only need to allocate space
 * for them if the input is not already double. Otherwise, we just 
 * have them point to the appropriate locations in ulap and vlap.
 */
  if(type_ulap != NCL_double) {
    tmp_ulap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ulap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for coercing ulap array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vlap != NCL_double) {
    tmp_vlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for coercing vlap array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  l3     = min(nlat,nlon/2);
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2 ));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon))+nlat*(4*l3+1);
  ldwork = 2*nlat*(nlat+1)+1;
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lvhsgc = l3*l2*(2*nlat-l3+1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wvhagc == NULL || wvhsgc == NULL || br == NULL || bi == NULL ||
      cr == NULL || ci == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ilapvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  scalesqrd = pow(scale,2.);         /* radius of earth**2 */
  
  for(i = 0; i < nt; i++ ) {
    if(type_ulap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of ulap (tmp_ulap) to double.
 */
      coerce_subset_input_double(ulap,tmp_ulap,index_uv,type_ulap,
                                 nlatnlon,0,&missing_ulap,&missing_dulap);
    }
    else {
/*
 * Point tmp_ulap to appropriate location in ulap.
 */
      tmp_ulap = &((double*)ulap)[index_uv];
    }

    if(type_vlap != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vlap (tmp_vlap) to double.
 */
      coerce_subset_input_double(vlap,tmp_vlap,index_uv,type_vlap,
                                 nlatnlon,0,&missing_vlap,&missing_dvlap);
    }
    else {
/*
 * Point tmp_vlap to appropriate location in vlap.
 */
      tmp_vlap = &((double*)vlap)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_ulap = contains_missing(tmp_ulap,nlatnlon,has_missing_ulap,
                                          missing_dulap.doubleval);
    found_missing_vlap = contains_missing(tmp_vlap,nlatnlon,has_missing_vlap,
                                          missing_dvlap.doubleval);
    if(found_missing_ulap || found_missing_vlap) {
      nmiss++;
/*
 * Set all elements of the 2D grids to missing values, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_dv.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_ulap,tmp_vlap,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork,&ildwork,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_vlap,tmp_ulap,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("ilapvg","vhagc",&ier,&jer,&ker,&mer,6,5);
/* 
 * Compute the vector laplacian using the vector spherical harmonic 
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork,&ildwork,
			       &jer);
      NGCALLF(divlapgc,DIVLAPGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                                   &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                   wvhsgc,&ilvhsgc,work3,&ilwork3,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("ilapvg","vhsgci,ivlapgc",&ier,&jer,&ker,&mer,
                                 6,14);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ulap,tmp_vlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scalesqrd,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scalesqrd,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"ilapvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wvhagc);
  NclFree(wvhsgc);
  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);

  if(type_ulap != NCL_double) NclFree(tmp_ulap);
  if(type_vlap != NCL_double) NclFree(tmp_vlap);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes lapsf_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *tmp_zlap = NULL;
  NclBasicDataTypes type_zlap;
  NclScalar missing_zlap, missing_dzlap;
  int has_missing_zlap;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t total_size_in;
  ng_size_t index_z;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshaec, lshsec;
  double *work1, *work2, *work3, *wshaec, *wshsec, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output array.
 */
  zlap = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
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
/*
 * Output must be float or double.
 */
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
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,&missing_dzlap,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshaec == NULL || wshsec == NULL || a == NULL || b == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */
 
  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_zlap == NCL_double) {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_zlap) {
        set_subset_output_missing(zlap,index_z,type_zlap,nlatnlon,
                                  missing_dzlap.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,a,b,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapsf","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the laplacian
 */ 
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,
			       &jer);
      NGCALLF(dslapec,DSLAPEC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,
                                 a,b,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                                 &ker);
                               
      NGCALLF(dchkerr,DCHKERR)("lapsf","shseci+slapec",&ier,&jer,&ker,&mer,
                                 5,13);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_zlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_zlap == NCL_float) {
        coerce_output_float_only(zlap,tmp_zlap,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshaec);
  NclFree(wshsec);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zlap != NCL_double) NclFree(tmp_zlap);

  return(NhlNOERROR);
}


NhlErrorTypes lapsF_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclScalar missing_z, missing_dz, missing_rz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *tmp_zlap = NULL;
  NclScalar missing_zlap, missing_dzlap;
  NclBasicDataTypes type_zlap;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t total_size_in;
  ng_size_t index_z;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshaec, lshsec;
  double *work1, *work2, *work3, *wshaec, *wshsec, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
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
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output array, if not already double.
 */
  if(type_z != NCL_double) {
    type_zlap = NCL_float;
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    zlap = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_zlap == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_zlap = NCL_double;
    zlap = (void*)calloc(total_size_in,sizeof(double));
  }
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_z) {
    if(type_zlap == NCL_double) missing_zlap = missing_dz;
    else                        missing_zlap = missing_rz;
    missing_dzlap = missing_dz;
  }

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat+1;
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshaec == NULL || wshsec == NULL || a == NULL || b == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */
 
  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_zlap == NCL_double) {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(zlap,index_z,type_zlap,nlatnlon,
                                missing_dzlap.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/* 
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,a,b,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapsF","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the laplacian
 */ 
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,
			       &jer);
      NGCALLF(dslapec,DSLAPEC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,
                                 a,b,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                                 &ker);

      NGCALLF(dchkerr,DCHKERR)("lapsF","shseci+slapec",&ier,&jer,&ker,&mer,
                                 5,13);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_zlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_zlap == NCL_float) {
        coerce_output_float_only(zlap,tmp_zlap,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshaec);
  NclFree(wshsec);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zlap != NCL_double) NclFree(tmp_zlap);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(zlap,ndims_z,dsizes_z,&missing_zlap,type_zlap,0));
  }
  else {
    return(NclReturnValue(zlap,ndims_z,dsizes_z,NULL,type_zlap,0));
  }
}


NhlErrorTypes lapsg_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclScalar missing_z, missing_dz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *tmp_zlap = NULL;
  NclBasicDataTypes type_zlap;
  NclScalar missing_zlap, missing_dzlap;
  int has_missing_zlap;
  int ndims_zlap;
  ng_size_t dsizes_zlap[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t total_size_in;
  ng_size_t index_z;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshagc, lshsgc;
  double *work1, *work2, *work3, *wshagc, *wshsgc, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output array.
 */
  zlap = (void*)NclGetArgValue(
           1,
           2,
           &ndims_zlap, 
           dsizes_zlap,
           &missing_zlap,
           &has_missing_zlap,
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
/*
 * Output must be float or double.
 */
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
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,NULL);
  coerce_missing(type_zlap,has_missing_zlap,&missing_zlap,&missing_dzlap,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_zlap != NCL_double) {
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_zlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for coercing zlap array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat*(nlat+4);
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshagc == NULL || wshsgc == NULL || a == NULL || b == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_zlap == NCL_double) {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_zlap) {
        set_subset_output_missing(zlap,index_z,type_zlap,nlatnlon,
                                  missing_dzlap.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,&jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,a,b,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapsg","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the laplacian
 */ 
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
      NGCALLF(dslapgc,DSLAPGC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,
                                 a,b,&imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,
                                 &ker);

      NGCALLF(dchkerr,DCHKERR)("lapsg","shsgci+slapgc",&ier,&jer,&ker,&mer,
                                 5,13);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_zlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_zlap == NCL_float) {
        coerce_output_float_only(zlap,tmp_zlap,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshagc);
  NclFree(wshsgc);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zlap != NCL_double) NclFree(tmp_zlap);

  return(NhlNOERROR);
}


NhlErrorTypes lapsG_W( void )
{
/*
 * Input array variables
 */
  void *z;
  double *tmp_z = NULL;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int ndims_z;
  NclScalar missing_z, missing_dz, missing_rz;
  NclBasicDataTypes type_z;
  int has_missing_z, found_missing_z;
  ng_size_t nt, nlat, nlon, nlatnlon;
/*
 * Output array variables
 */
  void *zlap;
  double *tmp_zlap = NULL;
  NclScalar missing_zlap, missing_dzlap;
  NclBasicDataTypes type_zlap;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t total_size_in;
  ng_size_t index_z;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lshagc, lshsgc;
  double *work1, *work2, *work3, *wshagc, *wshsgc, *a, *b, *dwork;
  int inlon;
  int inlat;
  int ilshagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
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
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_z is just big enough to hold a 2-dimensional subsection of the
 * z array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in z.
 */
  if(type_z != NCL_double) {
    tmp_z = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_z == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for coercing z array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate space for temporary output array, if not already double.
 */
  if(type_z != NCL_double) {
    type_zlap = NCL_float;
    tmp_zlap = (double*)calloc(nlatnlon,sizeof(double));
    zlap = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_zlap == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_zlap = NCL_double;
    zlap = (void*)calloc(total_size_in,sizeof(double));
  }
  if( zlap == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_z) {
    if(type_zlap == NCL_double) missing_zlap = missing_dz;
    else                        missing_zlap = missing_rz;
    missing_dzlap = missing_dz;
  }

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+2*l1+1);
  ldwork = nlat*(nlat+4);
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wshagc == NULL || wshsgc == NULL || a == NULL || b == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapsG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_z = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_z != NCL_double) {
/*
 * Coerce nlat x nlon subsection of z (tmp_z) to double.
 */
      coerce_subset_input_double(z,tmp_z,index_z,type_z,
                                 nlatnlon,0,&missing_z,&missing_dz);
    }
    else {
/*
 * Point tmp_z to appropriate location in z.
 */
      tmp_z = &((double*)z)[index_z];
    }
    if(type_zlap == NCL_double) {
/*
 * Point tmp_zlap to appropriate location in zlap.
 */
      tmp_zlap = &((double*)zlap)[index_z];
    }
/*
 * Check for missing values.
 */
    found_missing_z = contains_missing(tmp_z,nlatnlon,has_missing_z,
                                        missing_dz.doubleval);
    if(found_missing_z) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(zlap,index_z,type_zlap,nlatnlon,
                                missing_dzlap.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_z,work1);
/* 
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "z" (a scalar function)
 */ 
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,&jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_z,&iidvw,&ijdvw,a,b,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapsG","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the laplacian
 */ 
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
      NGCALLF(dslapgc,DSLAPGC)(&inlat,&inlon,&isym,&one,tmp_zlap,&iidvw,&ijdvw,
                                 a,b,&imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,
                                 &ker);

      NGCALLF(dchkerr,DCHKERR)("lapsG","shsgci+slapgc",&ier,&jer,&ker,&mer,
                                 5,13);
/*
 * Transform from math coordinates to geophysical coordinates
 *  (math) nlat is the first dim
 */ 
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_z,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_zlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_zlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_zlap == NCL_float) {
        coerce_output_float_only(zlap,tmp_zlap,nlatnlon,index_z);
      }
    }
    index_z += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wshagc);
  NclFree(wshsgc);
  NclFree(a);
  NclFree(b);

  if(type_z != NCL_double) NclFree(tmp_z);
  if(type_zlap != NCL_double) NclFree(tmp_zlap);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapsG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(zlap,ndims_z,dsizes_z,&missing_zlap,type_zlap,0));
  }
  else {
    return(NclReturnValue(zlap,ndims_z,dsizes_z,NULL,type_zlap,0));
  }
}

NhlErrorTypes lapvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void *ulap, *vlap;
  double *tmp_ulap = NULL;
  double *tmp_vlap = NULL;
  ng_size_t dsizes_ulap[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
  NclScalar missing_ulap, missing_dulap;
  NclScalar missing_vlap, missing_dvlap;
  int has_missing_ulap, has_missing_vlap;
  NclBasicDataTypes type_ulap, type_vlap;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lvhaec, lvhsec;
  double *work1, *work2, *work3, *wvhaec, *wvhsec, *br, *bi, *cr, *ci, *dwork;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (double*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  ulap = (double*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           &type_ulap,
           1);
  vlap = (double*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_ulap,has_missing_ulap,&missing_ulap,&missing_dulap,
                 NULL);
  coerce_missing(type_vlap,has_missing_vlap,&missing_vlap,&missing_dvlap,
                 NULL);
/*
 * Allocate space for temporary input. The temporary arrays tmp_u
 * and tmp_v are just big enough to hold a 2-dimensional
 * subsection of the u, v array. We only need to allocate space
 * for them if the input is not already double. Otherwise, we just 
 * have them point to the appropriate locations in u and v.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ulap != NCL_double) {
    tmp_ulap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ulap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for coercing ulap array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vlap != NCL_double) {
    tmp_vlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for coercing vlap array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+1)+4*(l1*nlat);
  ldwork = 2*(nlat+2);
  lvhaec = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));
  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wvhaec == NULL || wvhsec == NULL || br == NULL || bi == NULL ||
      cr == NULL || ci == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,
                                 nlatnlon,0,&missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }


    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,
                                 nlatnlon,0,&missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }

    if(type_ulap == NCL_double) {
/*
 * Point tmp_ulap to appropriate location in ulap.
 */
      tmp_ulap = &((double*)ulap)[index_uv];
    }
    if(type_vlap == NCL_double) {
/*
 * Point tmp_vlap to appropriate location in vlap.
 */
      tmp_vlap = &((double*)vlap)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of the 2D grids to missing values, if a missing
 * value exists.
 */
      if(has_missing_ulap) {
        set_subset_output_missing(ulap,index_uv,type_ulap,nlatnlon,
                                  missing_dulap.doubleval);
      }
      if(has_missing_vlap) {
        set_subset_output_missing(vlap,index_uv,type_vlap,nlatnlon,
                                  missing_dvlap.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork,&ildwork,&jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapvf","vhaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the vector laplacian using the vector spherical harmonic 
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork,&ildwork,
			       &jer);
      NGCALLF(dvlapec,DVLAPEC)(&inlat,&inlon,&isym,&one,tmp_vlap,tmp_ulap,
                                 &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                 wvhsec,&ilvhsec,work3,&ilwork3,&ker);
                           
      NGCALLF(dchkerr,DCHKERR)("lapvf","vhseci,vlapec",&ier,&jer,&ker,&mer,
                                 5,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ulap,tmp_vlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ulap,&invscalesqrd,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_ulap == NCL_float) {
        coerce_output_float_only(ulap,tmp_ulap,nlatnlon,index_uv);
      }
      if(type_vlap == NCL_float) {
        coerce_output_float_only(vlap,tmp_vlap,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wvhaec);
  NclFree(wvhsec);
  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_ulap != NCL_double) NclFree(tmp_ulap);
  if(type_vlap != NCL_double) NclFree(tmp_vlap);

  return(NhlNOERROR);
}


NhlErrorTypes lapvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void *ulap, *vlap;
  double *tmp_ulap = NULL;
  double *tmp_vlap = NULL;
  ng_size_t dsizes_ulap[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_vlap[NCL_MAX_DIMENSIONS];
  int ndims_ulap, ndims_vlap;
  NclScalar missing_ulap, missing_dulap;
  NclScalar missing_vlap, missing_dvlap;
  int has_missing_ulap, has_missing_vlap;
  NclBasicDataTypes type_ulap, type_vlap;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
  double invscalesqrd;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork, lvhagc, lvhsgc;
  double *work1, *work2, *work3, *wvhagc, *wvhsgc, *br, *bi, *cr, *ci, *dwork;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (double*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  ulap = (double*)NclGetArgValue(
           2,
           4,
           &ndims_ulap, 
           dsizes_ulap,
           &missing_ulap,
           &has_missing_ulap,
           &type_ulap,
           1);
  vlap = (double*)NclGetArgValue(
           3,
           4,
           &ndims_vlap, 
           dsizes_vlap,
           &missing_vlap,
           &has_missing_vlap,
           &type_vlap,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_ulap,has_missing_ulap,&missing_ulap,&missing_dulap,
                 NULL);
  coerce_missing(type_vlap,has_missing_vlap,&missing_vlap,&missing_dvlap,
                 NULL);
/*
 * Allocate space for temporary input. The temporary arrays tmp_u
 * and tmp_v are just big enough to hold a 2-dimensional
 * subsection of the u, v array. We only need to allocate space
 * for them if the input is not already double. Otherwise, we just 
 * have them point to the appropriate locations in u and v.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ulap != NCL_double) {
    tmp_ulap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ulap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for coercing ulap array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vlap != NCL_double) {
    tmp_vlap = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vlap == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for coercing vlap array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  lwork2 = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2 ));
  lwork3 = nlat*(2*nlon+max(6*l2,nlon)+1)+4*(l1*nlat);
  ldwork = 2*nlat*(nlat+1)+1;
  lvhagc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lvhsgc = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));
  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork == NULL || 
      wvhagc == NULL || wvhsgc == NULL || br == NULL || bi == NULL ||
      cr == NULL || ci == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lapvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork = (int) ldwork;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscalesqrd = pow(1./scale,2.);       /* (1/(radius of earth))**2 */
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,
                                 nlatnlon,0,&missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }

    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,
                                 nlatnlon,0,&missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }

    if(type_ulap == NCL_double) {
/*
 * Point tmp_ulap to appropriate location in ulap.
 */
      tmp_ulap = &((double*)ulap)[index_uv];
    }
    if(type_vlap == NCL_double) {
/*
 * Point tmp_vlap to appropriate location in vlap.
 */
      tmp_vlap = &((double*)vlap)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of the 2D grids to missing values, if a missing
 * value exists.
 */
      if(has_missing_ulap) {
        set_subset_output_missing(ulap,index_uv,type_ulap,nlatnlon,
                                  missing_dulap.doubleval);
      }
      if(has_missing_vlap) {
        set_subset_output_missing(vlap,index_uv,type_vlap,nlatnlon,
                                  missing_dvlap.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork,&ildwork,&jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapvg","vhagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * Compute the vector laplacian using the vector spherical harmonic 
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork,&ildwork,
			       &jer);
      NGCALLF(dvlapgc,DVLAPGC)(&inlat,&inlon,&isym,&one,tmp_vlap,tmp_ulap,
                                 &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                 wvhsgc,&ilvhsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("lapvg","vhsgci,vlapgc",&ier,&jer,&ker,&mer,
                                 5,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ulap,tmp_vlap,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ulap,&invscalesqrd,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vlap,&invscalesqrd,&ner);
/*
 * Coerce output back to float if necessary.
 */
      if(type_ulap == NCL_float) {
        coerce_output_float_only(ulap,tmp_ulap,nlatnlon,index_uv);
      }
      if(type_vlap == NCL_float) {
        coerce_output_float_only(vlap,tmp_vlap,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lapvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork);
  NclFree(wvhagc);
  NclFree(wvhsgc);
  NclFree(bi);
  NclFree(br);
  NclFree(ci);
  NclFree(cr);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_ulap != NCL_double) NclFree(tmp_ulap);
  if(type_vlap != NCL_double) NclFree(tmp_vlap);

  return(NhlNOERROR);
}



NhlErrorTypes uv2sfvpf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *sf, *vp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  NclBasicDataTypes type_sf, type_vp;
  int ndims_sf;
  ng_size_t dsizes_sf[NCL_MAX_DIMENSIONS];
  int ndims_vp;
  ng_size_t dsizes_vp[NCL_MAX_DIMENSIONS];
  NclScalar missing_sf, missing_dsf, missing_vp, missing_dvp;
  int has_missing_sf, has_missing_vp;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  sf = (void*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           &missing_sf,
           &has_missing_sf,
           &type_sf,
           1);
  vp = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           &missing_vp,
           &has_missing_vp,
           &type_vp,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_sf,has_missing_sf,&missing_sf,&missing_dsf,NULL);
  coerce_missing(type_vp,has_missing_vp,&missing_vp,&missing_dvp,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, sf, and vp.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_sf != NCL_double) {
    tmp_sf = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_sf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for coercing sf array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vp != NCL_double) {
    tmp_vp = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for coercing vp array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = max(nlat+1,nlat*((nlon+max(3*l2,nlon))+2*l1+1));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_sf == NCL_double) {
/*
 * Point tmp_sf to appropriate location in sf.
 */
      tmp_sf = &((double*)sf)[index_uv];
    }
    if(type_vp == NCL_double) {
/*
 * Point tmp_vp to appropriate location in vp.
 */
      tmp_vp = &((double*)vp)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_sf) {
        set_subset_output_missing(sf,index_uv,type_sf,nlatnlon,
                                  missing_dsf.doubleval);
      }
      if(has_missing_vp) {
        set_subset_output_missing(vp,index_uv,type_vp,nlatnlon,
                                  missing_dvp.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpf","vhaec",&ier,&jer,&ker,&mer,8,5);
  
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(dsfvpec,DSFVPEC)(&inlat,&inlon,&isym,&one,tmp_sf,tmp_vp,
                                 &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                 wshsec,&ilshsec,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpf","sfvpec+shseci",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_sf,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vp,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_sf  == NCL_float) {
        coerce_output_float_only(sf,tmp_sf,nlatnlon,index_uv);
      }
      if(type_vp  == NCL_float) {
        coerce_output_float_only(vp,tmp_vp,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2sfvpf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_sf != NCL_double) NclFree(tmp_sf);
  if(type_vp != NCL_double) NclFree(tmp_vp);

  return(NhlNOERROR);
}


NhlErrorTypes uv2sfvpF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *sfvp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  NclBasicDataTypes type_sfvp;
  int ndims_sfvp;
  ng_size_t *dsizes_sfvp;
  NclScalar missing_sfvp, missing_dsfvp;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ret;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv, index_sf, index_vp;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, sf, and vp.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_u != NCL_double && type_v != NCL_double) {
    type_sfvp = NCL_float;
    tmp_sf    = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vp    = (double*)calloc(nlatnlon,sizeof(double));
    sfvp      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_sf == NULL || tmp_vp == NULL || sfvp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_sfvp = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_dsfvp.doubleval = (double)missing_sfvp.floatval;
  }
  else {
    type_sfvp = NCL_double;
    sfvp      = (void*)calloc(2*total_size_in,sizeof(double));
    if(sfvp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dsfvp = missing_sfvp = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_sfvp  = ndims_u + 1;
  dsizes_sfvp = (ng_size_t*)calloc(ndims_sfvp,sizeof(ng_size_t));  
  dsizes_sfvp[0] = 2;
  for(i = 1; i <= ndims_u; i++ ) dsizes_sfvp[i] = dsizes_u[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = max(nlat+1,nlat*((nlon+max(3*l2,nlon))+2*l1+1));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_sf = index_uv = nmiss = 0;
  index_vp = total_size_in;
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_sfvp == NCL_double) {
      tmp_sf = &((double*)sfvp)[index_sf];
      tmp_vp = &((double*)sfvp)[index_vp];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(sfvp,index_sf,type_sfvp,nlatnlon,
                                missing_dsfvp.doubleval);
      set_subset_output_missing(sfvp,index_vp,type_sfvp,nlatnlon,
                                missing_dsfvp.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpF","vhaec",&ier,&jer,&ker,&mer,8,5);

      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(dsfvpec,DSFVPEC)(&inlat,&inlon,&isym,&one,tmp_sf,tmp_vp,
                                 &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                 wshsec,&ilshsec,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpF","sfvpec+shseci",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_sf,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vp,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_sfvp == NCL_float) {
        coerce_output_float_only(sfvp,tmp_sf,nlatnlon,index_sf);
        coerce_output_float_only(sfvp,tmp_vp,nlatnlon,index_vp);
      }
    }
    index_sf = index_uv += nlatnlon;
    index_vp += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_sfvp != NCL_double) {
    NclFree(tmp_sf);
    NclFree(tmp_vp);
  }

/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2sfvpF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(sfvp,ndims_sfvp,dsizes_sfvp,&missing_sfvp,
                          type_sfvp,0);
  }
  else {
    ret = NclReturnValue(sfvp,ndims_sfvp,dsizes_sfvp,NULL,type_sfvp,0);
  }
  NclFree(dsizes_sfvp);
  return(ret);
}


NhlErrorTypes uv2sfvpg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *sf, *vp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  NclBasicDataTypes type_sf, type_vp;
  int ndims_sf;
  ng_size_t dsizes_sf[NCL_MAX_DIMENSIONS];
  int ndims_vp;
  ng_size_t dsizes_vp[NCL_MAX_DIMENSIONS];
  NclScalar missing_sf, missing_dsf, missing_vp, missing_dvp;
  int has_missing_sf, has_missing_vp;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  sf = (void*)NclGetArgValue(
           2,
           4,
           &ndims_sf, 
           dsizes_sf,
           &missing_sf,
           &has_missing_sf,
           &type_sf,
           1);
  vp = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vp, 
           dsizes_vp,
           &missing_vp,
           &has_missing_vp,
           &type_vp,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_sf,has_missing_sf,&missing_sf,&missing_dsf,NULL);
  coerce_missing(type_vp,has_missing_vp,&missing_vp,&missing_dvp,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, sf, and vp.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_sf != NCL_double) {
    tmp_sf = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_sf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for coercing sf array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vp != NCL_double) {
    tmp_vp = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for coercing vp array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2 ));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*((nlon+max(3*l2,nlon))+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_sf == NCL_double) {
/*
 * Point tmp_sf to appropriate location in sf.
 */
      tmp_sf = &((double*)sf)[index_uv];
    }
    if(type_vp == NCL_double) {
/*
 * Point tmp_vp to appropriate location in vp.
 */
      tmp_vp = &((double*)vp)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_sf) {
        set_subset_output_missing(sf,index_uv,type_sf,nlatnlon,
                                  missing_dsf.doubleval);
      }
      if(has_missing_vp) {
        set_subset_output_missing(vp,index_uv,type_vp,nlatnlon,
                                  missing_dvp.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpg","vhagc",&ier,&jer,&ker,&mer,8,5);

      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(dsfvpgc,DSFVPGC)(&inlat,&inlon,&isym,&one,tmp_sf,tmp_vp,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wshsgc,&ilshsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpg","sfvpgc+shsgci",&ier,&jer,&ker,
                                &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_sf,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vp,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_sf == NCL_float) {
        coerce_output_float_only(sf,tmp_sf,nlatnlon,index_uv);
      }
      if(type_vp == NCL_float) {
        coerce_output_float_only(vp,tmp_vp,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2sfvpg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_sf != NCL_double) NclFree(tmp_sf);
  if(type_vp != NCL_double) NclFree(tmp_vp);

  return(NhlNOERROR);
}


NhlErrorTypes uv2sfvpG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *sfvp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  NclBasicDataTypes type_sfvp;
  int ndims_sfvp;
  ng_size_t *dsizes_sfvp;
  NclScalar missing_sfvp, missing_dsfvp;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ret;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv, index_sf, index_vp;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, sf, and vp.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_u != NCL_double && type_v != NCL_double) {
    type_sfvp = NCL_float;
    tmp_sf    = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vp    = (double*)calloc(nlatnlon,sizeof(double));
    sfvp      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_sf == NULL || tmp_vp == NULL || sfvp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_sfvp = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_dsfvp.doubleval = (double)missing_sfvp.floatval;
  }
  else {
    type_sfvp = NCL_double;
    sfvp      = (void*)calloc(2*total_size_in,sizeof(double));
    if(sfvp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dsfvp = missing_sfvp = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_sfvp  = ndims_u + 1;
  dsizes_sfvp = (ng_size_t*)calloc(ndims_sfvp,sizeof(ng_size_t));  
  dsizes_sfvp[0] = 2;
  for(i = 1; i <= ndims_u; i++ ) dsizes_sfvp[i] = dsizes_u[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2 ));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*((nlon+max(3*l2,nlon))+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2sfvpG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_sf = index_uv = nmiss = 0;
  index_vp = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_sfvp == NCL_double) {
      tmp_sf = &((double*)sfvp)[index_sf];
      tmp_vp = &((double*)sfvp)[index_vp];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(sfvp,index_sf,type_sfvp,nlatnlon,
                                missing_dsfvp.doubleval);
      set_subset_output_missing(sfvp,index_vp,type_sfvp,nlatnlon,
                                missing_dsfvp.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpG","vhagc",&ier,&jer,&ker,&mer,8,5);

      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(dsfvpgc,DSFVPGC)(&inlat,&inlon,&isym,&one,tmp_sf,tmp_vp,
                                 &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                                 wshsgc,&ilshsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2sfvpG","sfvpgc+shsgci",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_sf,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vp,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_sfvp == NCL_float) {
        coerce_output_float_only(sfvp,tmp_sf,nlatnlon,index_sf);
        coerce_output_float_only(sfvp,tmp_vp,nlatnlon,index_vp);
      }
    }
    index_sf = index_uv += nlatnlon;
    index_vp += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_sfvp != NCL_double) {
    NclFree(tmp_sf);
    NclFree(tmp_vp);
  }

/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2sfvpG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(sfvp,ndims_sfvp,dsizes_sfvp,&missing_sfvp,
                          type_sfvp,0);
  }
  else {
    ret = NclReturnValue(sfvp,ndims_sfvp,dsizes_sfvp,NULL,type_sfvp,0);
  }
  NclFree(dsizes_sfvp);
  return(ret);
}


NhlErrorTypes lderuvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *uy, *vy;
  double *tmp_uy = NULL;
  double *tmp_vy = NULL;
  NclBasicDataTypes type_uy, type_vy;
  int ndims_uy;
  ng_size_t dsizes_uy[NCL_MAX_DIMENSIONS];
  int ndims_vy;
  ng_size_t dsizes_vy[NCL_MAX_DIMENSIONS];
  int has_missing_uy, has_missing_vy;
  NclScalar missing_uy, missing_vy, missing_duy, missing_dvy;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lwvts;
  double *work1, *work2, *work3, *wvhaec, *wvts, *br, *bi, *cr, *ci;
  double *dwork1, *dwork2;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilwvts;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  uy = (void*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           &missing_uy,
           &has_missing_uy,
           &type_uy,
           1);
  vy = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           &missing_vy,
           &has_missing_vy,
           &type_vy,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
/*
 * Output must be float or double.
 */
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_uy,has_missing_uy,&missing_uy,&missing_duy,NULL);
  coerce_missing(type_vy,has_missing_vy,&missing_vy,&missing_dvy,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, uy, and vy.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_uy != NCL_double) {
    tmp_uy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_uy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for coercing uy array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vy != NCL_double) {
    tmp_vy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for coercing vy array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  ityp   = 0;
  idvw   = nlat;
  jdvw   = nlon;
  ndab   = nlat;
  mdab   = min(nlat,(nlon+2)/2);
  l1     = min(nlat,(nlon+2)/2);
  l2     = (nlat+1)/2;
  lwork1 = nlatnlon;
  /*  lwork2  = max(4*(nlat+1),nlat*(2*nlon*nt+max(6*l2,nlon)));*/
  lwork2 = nlat*(2*nt*nlon+max(6*l2,nlon));
  lwork3  = nlat*(2*nt*nlon+max(6*l2,nlon));
  ldwork1 = 2*(nlat+2);
  ldwork2 = 2*(nlat+1);
  lwvts   = 4*nlat*l2 +3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon +15;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  wvts   = (double*)calloc(lwvts,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || br == NULL || bi == NULL || 
      cr == NULL || ci == NULL || wvts == NULL || wvhaec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lwvts > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilwvts = (int) lwvts;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;      /* 1/(radius of earth) */
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_uy == NCL_double) {
/*
 * Point tmp_uy to appropriate location in uy.
 */
      tmp_uy = &((double*)uy)[index_uv];
    }
    if(type_vy == NCL_double) {
/*
 * Point tmp_vy to appropriate location in vy.
 */
      tmp_vy = &((double*)vy)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_uy) {
        set_subset_output_missing(uy,index_uv,type_uy,nlatnlon,
                                  missing_duy.doubleval);
      }
      if(has_missing_vy) {
        set_subset_output_missing(vy,index_uv,type_vy,nlatnlon,
                                  missing_dvy.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&ityp,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lderuvf","vhaec",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 
      NGCALLF(dvtseci,DVTSECI)(&inlat,&inlon,wvts,&ilwvts,dwork2,&ildwork2,&jer);
      NGCALLF(dvtsec,DVTSEC)(&inlat,&inlon,&ityp,&one,tmp_vy,tmp_uy,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,wvts,&ilwvts,
                               work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("lderuvf","vtsec",&ier,&jer,&ker,&mer,7,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_uy,tmp_vy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_uy,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vy,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uy  == NCL_float) {
        coerce_output_float_only(uy,tmp_uy,nlatnlon,index_uv);
      }
      if(type_vy  == NCL_float) {
        coerce_output_float_only(vy,tmp_vy,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lderuvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wvts);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_uy != NCL_double) NclFree(tmp_uy);
  if(type_vy != NCL_double) NclFree(tmp_vy);

  return(NhlNOERROR);
}


NhlErrorTypes lderuvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *uy, *vy;
  double *tmp_uy = NULL;
  double *tmp_vy = NULL;
  NclBasicDataTypes type_uy, type_vy;
  int ndims_uy;
  ng_size_t dsizes_uy[NCL_MAX_DIMENSIONS];
  int ndims_vy;
  ng_size_t dsizes_vy[NCL_MAX_DIMENSIONS];
  int has_missing_uy, has_missing_vy;
  NclScalar missing_uy, missing_vy, missing_duy, missing_dvy;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lwvts;
  double *work1, *work2, *work3, *wvhagc, *wvts, *br, *bi, *cr, *ci;
  double *dwork1, *dwork2;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilwvts;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  uy = (void*)NclGetArgValue(
           2,
           4,
           &ndims_uy, 
           dsizes_uy,
           &missing_uy,
           &has_missing_uy,
           &type_uy,
           1);
  vy = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vy, 
           dsizes_vy,
           &missing_vy,
           &has_missing_vy,
           &type_vy,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
/*
 * Output must be float or double.
 */
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_uy,has_missing_uy,&missing_uy,&missing_duy,NULL);
  coerce_missing(type_vy,has_missing_vy,&missing_vy,&missing_dvy,NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, uy, and vy.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_uy != NCL_double) {
    tmp_uy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_uy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for coercing uy array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vy != NCL_double) {
    tmp_vy = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vy == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for coercing vy array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  ityp    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = 2*nlat*(2*nlon+3*l2);
  lwork3  = nlat*(2*nlon+max(6*l2,nlon));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+l2+15;
  lwvts   = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wvts   = (double*)calloc(lwvts,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || br == NULL || bi == NULL || 
      cr == NULL || ci == NULL || wvts == NULL || wvhagc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lwvts > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"lderuvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilwvts = (int) lwvts;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;      /* 1/(radius of earth) */
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_uy == NCL_double) {
/*
 * Point tmp_uy to appropriate location in uy.
 */
      tmp_uy = &((double*)uy)[index_uv];
    }
    if(type_vy == NCL_double) {
/*
 * Point tmp_vy to appropriate location in vy.
 */
      tmp_vy = &((double*)vy)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_uy) {
        set_subset_output_missing(uy,index_uv,type_uy,nlatnlon,
                                  missing_duy.doubleval);
      }
      if(has_missing_vy) {
        set_subset_output_missing(vy,index_uv,type_vy,nlatnlon,
                                  missing_dvy.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&ityp,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("lderuvg","vhagc",&ier,&jer,&ker,&mer,7,5);
/*
 * Compute derivative of (u,v) with respect to colatitude theta
 * [upon return: derivative of (u,v) with respect to latitude]
 */ 
      NGCALLF(dvtsgci,DVTSGCI)(&inlat,&inlon,wvts,&ilwvts,dwork2,&ildwork2,&jer);
      NGCALLF(dvtsgc,DVTSGC)(&inlat,&inlon,&ityp,&one,tmp_vy,tmp_uy,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,wvts,&ilwvts,
                               work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("lderuvg","vtsgc",&ier,&jer,&ker,&mer,7,5);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_uy,tmp_vy,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_uy,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vy,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uy  == NCL_float) {
        coerce_output_float_only(uy,tmp_uy,nlatnlon,index_uv);
      }
      if(type_vy  == NCL_float) {
        coerce_output_float_only(vy,tmp_vy,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"lderuvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wvts);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_uy != NCL_double) NclFree(tmp_uy);
  if(type_vy != NCL_double) NclFree(tmp_vy);

  return(NhlNOERROR);
}


NhlErrorTypes uv2dvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_dv;
  NclScalar missing_dvo, missing_ddvo;
  int has_missing_dv;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output array.
 */
  dv = (void*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dv,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
/*
 * The output array must be float or double.
 */
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_dv,has_missing_dv,&missing_dvo,&missing_ddvo,NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_dv) {
        set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                  missing_ddvo.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvf","vhaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(ddivec,DDIVEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
                               br,bi,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvf","shseci+divec",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine.
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_dv  == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  return(NhlNOERROR);
}


NhlErrorTypes uv2dvF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv, missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_dv;
  NclScalar missing_dvo, missing_ddvo;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    type_dv = NCL_float;
    tmp_dv  = (double*)calloc(nlatnlon,sizeof(double));
    dv = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_dv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_dv = NCL_double;
    dv = (void*)calloc(total_size_in,sizeof(double));
  }
  if( dv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_u) {
    if(type_dv == NCL_double) missing_dvo = missing_du;
    else                      missing_dvo = missing_ru;
    missing_ddvo = missing_du;
  }
  else if(has_missing_v) {
    if(type_dv == NCL_double) missing_dvo = missing_dv;
    else                      missing_dvo = missing_rv;
    missing_ddvo = missing_dv;
  }

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                missing_ddvo.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvF","vhaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(ddivec,DDIVEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
                               br,bi,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvF","shseci+divec",&ier,&jer,&ker,&mer,
                                 6,12);

/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine.
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_dv  == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(dv,ndims_u,dsizes_u,&missing_dvo,type_dv,0));
  }
  else {
    return(NclReturnValue(dv,ndims_u,dsizes_u,NULL,type_dv,0));
  }
}


NhlErrorTypes uv2dvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_dv;
  NclScalar missing_dvo, missing_ddvo;
  int has_missing_dv;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output array.
 */
  dv = (void*)NclGetArgValue(
           2,
           3,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dv,
           &type_dv,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
/*
 * The output array must be float or double.
 */
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_dv,has_missing_dv,&missing_dvo,&missing_ddvo,NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and vort.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_dv) {
        set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                  missing_ddvo.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvg","vhagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(ddivgc,DDIVGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,
                               br,bi,&imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvg","shsgci+divgc",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_dv   == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  return(NhlNOERROR);
}


NhlErrorTypes uv2dvG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv, missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *dv;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_dv;
  NclScalar missing_dvo, missing_ddvo;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    type_dv = NCL_float;
    tmp_dv  = (double*)calloc(nlatnlon,sizeof(double));
    dv = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_dv == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_dv = NCL_double;
    dv = (void*)calloc(total_size_in,sizeof(double));
  }
  if( dv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_u) {
    if(type_dv == NCL_double) missing_dvo = missing_du;
    else                      missing_dvo = missing_ru;
    missing_ddvo = missing_du;
  }
  else if(has_missing_v) {
    if(type_dv == NCL_double) missing_dvo = missing_dv;
    else                      missing_dvo = missing_rv;
    missing_ddvo = missing_dv;
  }

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2dvG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                missing_ddvo.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvG","vhagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the divergence using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(ddivgc,DDIVGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,br,bi,
                               &imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2dvG","shsgci+divgc",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_dv == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2dvG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(dv,ndims_u,dsizes_u,&missing_dvo,type_dv,0));
  }
  else {
    return(NclReturnValue(dv,ndims_u,dsizes_u,NULL,type_dv,0));
  }
}

NhlErrorTypes uv2vrf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_vort;
  NclScalar missing_vort, missing_dvort;
  int has_missing_vort;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output array.
 */
  vort = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and vort.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vort != NCL_double) {
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vort == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for coercing vort array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  ndab    = nlat;
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  mdab    = min(nlat,(nlon+2)/2);
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(     lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(    ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wvhaec = (double*)calloc(     lvhaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  br     = (double*)calloc(  mdab*ndab,sizeof(double));
  bi     = (double*)calloc(  mdab*ndab,sizeof(double));
  cr     = (double*)calloc(  mdab*ndab,sizeof(double));
  ci     = (double*)calloc(  mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */
  

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vort == NCL_double) {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_vort) {
        set_subset_output_missing(vort,index_uv,type_vort,nlatnlon,
                                  missing_dvort.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrf","vhaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,
			       dwork2,&ildwork2,&jer);
      NGCALLF(dvrtec,DVRTEC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               cr,ci,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrf","shseci+vrtec",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine.
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vort,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vort == NCL_float) {
        coerce_output_float_only(vort,tmp_vort,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vort != NCL_double) NclFree(tmp_vort);

  return(NhlNOERROR);
}


NhlErrorTypes uv2vrF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv, missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  NclBasicDataTypes type_vort;
  NclScalar missing_vort, missing_dvort;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and vort.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    type_vort = NCL_float;
    tmp_vort  = (double*)calloc(nlatnlon,sizeof(double));
    vort = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_vort == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_vort = NCL_double;
    vort = (void*)calloc(total_size_in,sizeof(double));
  }
  if( vort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_u) {
    if(type_vort == NCL_double) missing_vort = missing_du;
    else                        missing_vort = missing_ru;
    missing_dvort = missing_du;
  }
  else if(has_missing_v) {
    if(type_vort == NCL_double) missing_vort = missing_dv;
    else                        missing_vort = missing_rv;
    missing_dvort = missing_dv;
  }

/*
 * Allocate memory for work arrays.
 */
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  ndab    = nlat;
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  mdab    = min(nlat,(nlon+2)/2);
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(     lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(    ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wvhaec = (double*)calloc(     lvhaec,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  br     = (double*)calloc(  mdab*ndab,sizeof(double));
  bi     = (double*)calloc(  mdab*ndab,sizeof(double));
  cr     = (double*)calloc(  mdab*ndab,sizeof(double));
  ci     = (double*)calloc(  mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vort == NCL_double) {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(vort,index_uv,type_vort,nlatnlon,
                                missing_dvort.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrF","vhaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,
			       dwork2,&ildwork2,&jer);
      NGCALLF(dvrtec,DVRTEC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               cr,ci,&imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrF","shseci+vrtec",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine.
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vort,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vort == NCL_float) {
        coerce_output_float_only(vort,tmp_vort,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vort != NCL_double) NclFree(tmp_vort);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(vort,ndims_u,dsizes_u,&missing_vort,type_vort,0));
  }
  else {
    return(NclReturnValue(vort,ndims_u,dsizes_u,NULL,type_vort,0));
  }
}


NhlErrorTypes uv2vrg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_vort;
  NclScalar missing_vort, missing_dvort;
  int has_missing_vort;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           3,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output array.
 */
  vort = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and vort.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vort != NCL_double) {
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vort == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for coercing vort array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  /*  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon*nt+3*l2)); */
  lwork2  = 2*nlat*(2*nlon+3*l2);
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wshsgc == NULL || wvhagc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */
  
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vort == NCL_double) {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      if(has_missing_vort) {
        set_subset_output_missing(vort,index_uv,type_vort,nlatnlon,
                                  missing_dvort.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrg","vhagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(dvrtgc,DVRTGC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               cr,ci,&imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,
                               &ker);
                             
      NGCALLF(dchkerr,DCHKERR)("uv2vrg","shsgci+vrtgc",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vort,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vort == NCL_float) {
        coerce_output_float_only(vort,tmp_vort,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vort != NCL_double) NclFree(tmp_vort);

  return(NhlNOERROR);
}


NhlErrorTypes uv2vrG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv, missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  NclBasicDataTypes type_vort;
  NclScalar missing_vort, missing_dvort;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, and vort.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for output array.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    type_vort = NCL_float;
    tmp_vort  = (double*)calloc(nlatnlon,sizeof(double));
    vort = (void*)calloc(total_size_in,sizeof(float));
    if( tmp_vort == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_vort = NCL_double;
    vort = (void*)calloc(total_size_in,sizeof(double));
  }
  if( vort == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_u) {
    if(type_vort == NCL_double) missing_vort = missing_du;
    else                        missing_vort = missing_ru;
    missing_dvort = missing_du;
  }
  else if(has_missing_v) {
    if(type_vort == NCL_double) missing_vort = missing_dv;
    else                        missing_vort = missing_rv;
    missing_dvort = missing_dv;
  }

/*
 * Allocate memory for work arrays.
 */
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  ndab    = nlat;
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  mdab    = min(nlat,(nlon+2)/2);
  lwork1  = nlatnlon;
  lwork2  = 2*nlat*(2*nlon+3*l2);
  lwork3  = nlat*(nlon+max(3*l2,nlon)+2*l1+1);
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */
 
  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vort == NCL_double) {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of this 2D grid to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(vort,index_uv,type_vort,nlatnlon,
                                missing_dvort.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 * Note the order "vhagc(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrG","vhagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Compute the vorticity using the vector spherical harmonic 
 * coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(dvrtgc,DVRTGC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               cr,ci,&imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrG","shsgci+vrtgc",&ier,&jer,&ker,&mer,
                                 6,12);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vort,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vort == NCL_float) {
        coerce_output_float_only(vort,tmp_vort,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vort != NCL_double) NclFree(tmp_vort);

  if(nmiss) {
/*
 * If any input arrays contained missing values, print a warning message.
 */
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    return(NclReturnValue(vort,ndims_u,dsizes_u,&missing_vort,type_vort,0));
  }
  else {
    return(NclReturnValue(vort,ndims_u,dsizes_u,NULL,type_vort,0));
  }
}


NhlErrorTypes uv2vrdvf_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_vr, type_dv;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dvr, missing_dvo, missing_ddvo;
  int has_missing_vr, has_missing_dvo;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  vr = (void*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           1);
  dv = (void*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dvo,
           &type_dv,
           1);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dvo,&missing_dvo,&missing_ddvo,NULL);

/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, vr, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = max(nlat+1,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;      /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vr == NCL_double) {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_vr) {
        set_subset_output_missing(vr,index_uv,type_vr,nlatnlon,
                                  missing_dvr.doubleval);
      }
      if(has_missing_dvo) {
        set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                  missing_ddvo.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 *  Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvf","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(ddivec,DDIVEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,br,bi,
                               &imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,&ker);
      NGCALLF(dvrtec,DVRTEC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,cr,ci,
                               &imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvf","shseci+divec+vrtec",&ier,&jer,
                                 &ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vr  == NCL_float) {
        coerce_output_float_only(vr,tmp_vr,nlatnlon,index_uv);
      }
      if(type_dv  == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrdvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  return(NhlNOERROR);
}


NhlErrorTypes uv2vrdvF_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vrdv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_vrdv;
  int ndims_vrdv;
  ng_size_t *dsizes_vrdv;
  NclScalar missing_vrdv, missing_dvrdv;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ret;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv, index_vr, index_dv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhaec, lshsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhaec, *wshsec, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhaec;
  int ilshsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, vr, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_u != NCL_double && type_v != NCL_double) {
    type_vrdv = NCL_float;
    tmp_vr    = (double*)calloc(nlatnlon,sizeof(double));
    tmp_dv    = (double*)calloc(nlatnlon,sizeof(double));
    vrdv      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_vr == NULL || tmp_dv == NULL || vrdv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_vrdv = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_dvrdv.doubleval = (double)missing_vrdv.floatval;
  }
  else {
    type_vrdv = NCL_double;
    vrdv      = (void*)calloc(2*total_size_in,sizeof(double));
    if(vrdv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dvrdv = missing_vrdv = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_vrdv  = ndims_u + 1;
  dsizes_vrdv = (ng_size_t*)calloc(ndims_vrdv,sizeof(ng_size_t));  
  dsizes_vrdv[0] = 2;
  for(i = 1; i <= ndims_u; i++ ) dsizes_vrdv[i] = dsizes_u[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)));
  lwork3  = max(nlat+1,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*(nlat+2);
  ldwork2 = nlat+1;
  lvhaec  = 4*nlat*l2+3*max(l1-2,0)*(nlat+nlat-l1-1)+nlon+15;
  lshsec  = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsec = (double*)calloc(lshsec,sizeof(double));
  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhaec == NULL || wshsec == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhaec = (int) lvhaec;
  ilshsec = (int) lshsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vr = index_uv = nmiss = 0;
  index_dv = total_size_in;
  invscale = 1./scale;      /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vrdv == NCL_double) {
      tmp_vr = &((double*)vrdv)[index_vr];
      tmp_dv = &((double*)vrdv)[index_dv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(vrdv,index_vr,type_vrdv,nlatnlon,
                                missing_dvrdv.doubleval);
      set_subset_output_missing(vrdv,index_dv,type_vrdv,nlatnlon,
                                missing_dvrdv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 *  Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhaec,&ilvhaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvF","vhaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhaec'
 */
      NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(ddivec,DDIVEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,br,bi,
                               &imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,&ker);
      NGCALLF(dvrtec,DVRTEC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,cr,ci,
                               &imdab,&indab,wshsec,&ilshsec,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvF","shseci+divec+vrtec",&ier,&jer,
                                 &ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vrdv == NCL_float) {
        coerce_output_float_only(vrdv,tmp_vr,nlatnlon,index_vr);
        coerce_output_float_only(vrdv,tmp_dv,nlatnlon,index_dv);
      }
    }
    index_vr = index_uv += nlatnlon;
    index_dv += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhaec);
  NclFree(wshsec);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vrdv != NCL_double) {
    NclFree(tmp_vr);
    NclFree(tmp_dv);
  }

/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrdvF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(vrdv,ndims_vrdv,dsizes_vrdv,&missing_vrdv,
                          type_vrdv,0);
  }
  else {
    ret = NclReturnValue(vrdv,ndims_vrdv,dsizes_vrdv,NULL,type_vrdv,0);
  }
  NclFree(dsizes_vrdv);
  return(ret);
}


NhlErrorTypes uv2vrdvg_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_vr, type_dv;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dvr, missing_dvo, missing_ddvo;
  int has_missing_vr, has_missing_dvo;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);
/*
 * Get output arrays.
 */
  vr = (void*)NclGetArgValue(
           2,
           4,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           1);
  dv = (void*)NclGetArgValue(
           3,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dvo,
           &type_dv,
           1);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dvo,&missing_dvo,&missing_ddvo,NULL);


/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, vr, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;      /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vr == NCL_double) {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_uv];
    }
    if(type_dv == NCL_double) {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_vr) {
        set_subset_output_missing(vr,index_uv,type_vr,nlatnlon,
                                  missing_dvr.doubleval);
      }
      if(has_missing_dvo) {
        set_subset_output_missing(dv,index_uv,type_dv,nlatnlon,
                                  missing_ddvo.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 *  Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvg","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(ddivgc,DDIVGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,br,bi,
                               &imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,&ker);
      NGCALLF(dvrtgc,DVRTGC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,cr,ci,
                               &imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvg","shsgci+divgc+vrtgc",&ier,&jer,
                                 &ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vr  == NCL_float) {
        coerce_output_float_only(vr,tmp_vr,nlatnlon,index_uv);
      }
      if(type_dv  == NCL_float) {
        coerce_output_float_only(dv,tmp_dv,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrdvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);

  return(NhlNOERROR);
}


NhlErrorTypes uv2vrdvG_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclScalar missing_u, missing_v, missing_du, missing_dv;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v, found_missing_u, found_missing_v;
/*
 * Output array variables
 */
  void *vrdv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  NclBasicDataTypes type_vrdv;
  int ndims_vrdv;
  ng_size_t *dsizes_vrdv;
  NclScalar missing_vrdv, missing_dvrdv;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int ret;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t index_uv, index_vr, index_dv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lvhagc, lshsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wvhagc, *wshsgc, *br, *bi, *cr, *ci;
  int inlon;
  int inlat;
  int ilvhagc;
  int ilshsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);
/*
 * Allocate space for temporary input and output. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * input and output. We only need to allocate space for them if the
 * input/output is not already double. Otherwise, we just have them point
 * to the appropriate locations in u, v, vr, and dv.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_u != NCL_double && type_v != NCL_double) {
    type_vrdv = NCL_float;
    tmp_vr    = (double*)calloc(nlatnlon,sizeof(double));
    tmp_dv    = (double*)calloc(nlatnlon,sizeof(double));
    vrdv      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_vr == NULL || tmp_dv == NULL || vrdv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_vrdv = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_dvrdv.doubleval = (double)missing_vrdv.floatval;
  }
  else {
    type_vrdv = NCL_double;
    vrdv      = (void*)calloc(2*total_size_in,sizeof(double));
    if(vrdv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_dvrdv = missing_vrdv = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_vrdv  = ndims_u + 1;
  dsizes_vrdv = (ng_size_t*)calloc(ndims_vrdv,sizeof(ng_size_t));  
  dsizes_vrdv[0] = 2;
  for(i = 1; i <= ndims_u; i++ ) dsizes_vrdv[i] = dsizes_u[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+1)+2,2*nlat*(2*nlon+3*l2));
  lwork3  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)+2*l1+1));
  ldwork1 = 2*nlat*(nlat+1)+1;
  ldwork2 = nlat*(nlat+4);
  lvhagc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+l2+15;
  lshsgc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  wvhagc = (double*)calloc(lvhagc,sizeof(double));
  br     = (double*)calloc(mdab*ndab,sizeof(double));
  bi     = (double*)calloc(mdab*ndab,sizeof(double));
  cr     = (double*)calloc(mdab*ndab,sizeof(double));
  ci     = (double*)calloc(mdab*ndab,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || dwork1 == NULL || 
      dwork2 == NULL || wvhagc == NULL || wshsgc == NULL ||
      br == NULL || bi == NULL || cr == NULL || ci == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"uv2vrdvG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilvhagc = (int) lvhagc;
  ilshsgc = (int) lshsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vr = index_uv = nmiss = 0;
  index_dv = total_size_in;
  invscale = 1./scale;      /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_u != NCL_double) {
/*
 * Coerce nlat x nlon subsection of u (tmp_u) to double.
 */
      coerce_subset_input_double(u,tmp_u,index_uv,type_u,nlatnlon,0,
                                 &missing_u,&missing_du);
    }
    else {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v != NCL_double) {
/*
 * Coerce nlat x nlon subsection of v (tmp_v) to double.
 */
      coerce_subset_input_double(v,tmp_v,index_uv,type_v,nlatnlon,0,
                                 &missing_v,&missing_dv);
    }
    else {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
    if(type_vrdv == NCL_double) {
      tmp_vr = &((double*)vrdv)[index_vr];
      tmp_dv = &((double*)vrdv)[index_dv];
    }
/*
 * Check for missing values.
 */
    found_missing_u = contains_missing(tmp_u,nlatnlon,has_missing_u,
                                       missing_du.doubleval);
    found_missing_v = contains_missing(tmp_v,nlatnlon,has_missing_v,
                                       missing_dv.doubleval);
    if(found_missing_u || found_missing_v) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(vrdv,index_vr,type_vrdv,nlatnlon,
                                missing_dvrdv.doubleval);
      set_subset_output_missing(vrdv,index_dv,type_vrdv,nlatnlon,
                                missing_dvrdv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,tmp_u,tmp_v,work1);
/*
 * Perform vector spherical harmonic analysis to get coefficients 
 *  Note the order "vhaec(...,v,u,....)
 */
      NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                               &iidvw,&ijdvw,br,bi,cr,ci,&imdab,&indab,
                               wvhagc,&ilvhagc,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvG","vhagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the divergence using the vector spherical harmonic 
 *  coefficients br and bi computed by 'sub vhagc'
 */
      NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork2,&ildwork2,&jer);
      NGCALLF(ddivgc,DDIVGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,br,bi,
                               &imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,&ker);
      NGCALLF(dvrtgc,DVRTGC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,cr,ci,
                               &imdab,&indab,wshsgc,&ilshsgc,work3,&ilwork3,&ker);

      NGCALLF(dchkerr,DCHKERR)("uv2vrdvG","shsgci+divgc+vrtgc",&ier,&jer,
                                 &ker,&mer,8,18);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_dv,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_vrdv == NCL_float) {
        coerce_output_float_only(vrdv,tmp_vr,nlatnlon,index_vr);
        coerce_output_float_only(vrdv,tmp_dv,nlatnlon,index_dv);
      }
    }
    index_vr = index_uv += nlatnlon;
    index_dv += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wvhagc);
  NclFree(wshsgc);
  NclFree(br);
  NclFree(bi);
  NclFree(cr);
  NclFree(ci);

  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);
  if(type_vrdv != NCL_double) {
    NclFree(tmp_vr);
    NclFree(tmp_dv);
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"uv2vrdvG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(vrdv,ndims_vrdv,dsizes_vrdv,&missing_vrdv,
                          type_vrdv,0);
  }
  else {
    ret = NclReturnValue(vrdv,ndims_vrdv,dsizes_vrdv,NULL,type_vrdv,0);
  }
  NclFree(dsizes_vrdv);
  return(ret);
}

NhlErrorTypes vr2uvf_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing_vort;
/*
 * Output array variables
 */
  void *ur, *vr;
  double *tmp_ur = NULL;
  double *tmp_vr = NULL;
  int ndims_ur;
  ng_size_t dsizes_ur[NCL_MAX_DIMENSIONS];
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int has_missing_ur, has_missing_vr;
  NclScalar missing_ur, missing_vr, missing_dur, missing_dvr;
  NclBasicDataTypes type_ur, type_vr;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_vr;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  ur = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           &missing_ur,
           &has_missing_ur,
           &type_ur,
           1);
  vr = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
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
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 NULL);
  coerce_missing(type_ur,has_missing_ur,&missing_ur,&missing_dur,NULL);
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_vort is just big enough to hold a 2-dimensional subsection of the
 * vort array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in vort.
 */
  if(type_vort != NCL_double) {
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vort == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for coercing vort array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ur != NCL_double) {
    tmp_ur = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ur == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for coercing ur array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+1)/2 );
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)+2*l3+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshaec == NULL || wvhsec == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vr = nmiss = 0;
  
  for(i = 0; i < nt; i++ ) {
    if(type_vort != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vort (tmp_vort) to double.
 */
      coerce_subset_input_double(vort,tmp_vort,index_vr,type_vort,
                                 nlatnlon,0,&missing_vort,&missing_dvort);
    }
    else {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_vr];
    }
    if(type_ur == NCL_double) {
/*
 * Point tmp_ur to appropriate location in ur.
 */
      tmp_ur = &((double*)ur)[index_vr];
    }
    if(type_vr == NCL_double) {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_vr];
    }
/*
 * Check for missing values.
 */
    found_missing_vort = contains_missing(tmp_vort,nlatnlon,has_missing_vort,
                                        missing_dvort.doubleval);
    if(found_missing_vort) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_ur) {
        set_subset_output_missing(ur,index_vr,type_ur,nlatnlon,
                                  missing_dur.doubleval);
      }
      if(has_missing_vr) {
        set_subset_output_missing(vr,index_vr,type_vr,nlatnlon,
                                  missing_dvr.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vort,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_vort,
                               &iidvw,&ijdvw,a,b,&imdab,&indab,
                               wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvf","shaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Reconstruct the divergent (irrotational) wind components
 * note the argument order idivec(...,v,u,...)
 */
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,
			       &jer);
      NGCALLF(divrtec,DIVRTEC)(&inlat,&inlon,&isym,&one,tmp_vr,tmp_ur,
                                 &iidvw,&ijdvw,a,b,&imdab,&indab,
                                 wvhsec,&ilvhsec,work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvf","vhseci+ivrtec",&ier,&jer,&ker,&mer,
                                 6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ur,tmp_vr,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ur,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_ur == NCL_float) {
        coerce_output_float_only(ur,tmp_ur,nlatnlon,index_vr);
      }
      if(type_vr == NCL_float) {
        coerce_output_float_only(vr,tmp_vr,nlatnlon,index_vr);
      }
    }
    index_vr += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vr2uvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_vort != NCL_double) NclFree(tmp_vort);
  if(type_ur != NCL_double) NclFree(tmp_ur);
  if(type_vr != NCL_double) NclFree(tmp_vr);

  return(NhlNOERROR);
}


NhlErrorTypes vr2uvF_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort, missing_rvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing_vort;
/*
 * Output array variables
 */
  void *uvr;
  double *tmp_ur = NULL;
  double *tmp_vr = NULL;
  int ndims_uvr;
  ng_size_t *dsizes_uvr;
  NclScalar missing_uvr;
  NclBasicDataTypes type_uvr;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int ret;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_vr, index_ur, index_vort;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (void*)NclGetArgValue(
           0,
           1,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvF: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vort,ndims_vort,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 &missing_rvort);
/*
 * Allocate space for temporary input/output arrays. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * full arrays. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to their appropriate locations in vr.
 */
  if(type_vort != NCL_double) {
    type_uvr = NCL_float;
    tmp_ur   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vr   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    uvr      = (void*)calloc(2*total_size_in,sizeof(float));
    if(uvr == NULL || tmp_vort == NULL || tmp_ur == NULL || tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_vort) {
      missing_uvr = missing_rvort;
    }
  } 
  else {
    type_uvr = NCL_double;
    uvr      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uvr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_vort) {
      missing_uvr = missing_dvort;
    }
  }
/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uvr  = ndims_vort + 1;
  dsizes_uvr = (ng_size_t*)calloc(ndims_uvr,sizeof(ng_size_t));  
  dsizes_uvr[0] = 2;
  for(i = 1; i <= ndims_vort; i++ ) dsizes_uvr[i] = dsizes_vort[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+1)/2 );
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon)+2*l3+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshaec == NULL || wvhsec == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_ur = index_vort = nmiss = 0;
  index_vr = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_vort != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vort (tmp_vort) to double.
 */
      coerce_subset_input_double(vort,tmp_vort,index_vort,type_vort,
                                 nlatnlon,0,&missing_vort,&missing_dvort);
    }
    else {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_vort];
    }
    if(type_uvr == NCL_double) {
      tmp_ur = &((double*)uvr)[index_ur];
      tmp_vr = &((double*)uvr)[index_vr];
    }
/*
 * Check for missing values.
 */
    found_missing_vort = contains_missing(tmp_vort,nlatnlon,has_missing_vort,
                                          missing_dvort.doubleval);
    if(found_missing_vort) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(uvr,index_ur,type_uvr,nlatnlon,
                                missing_dvort.doubleval);
      set_subset_output_missing(uvr,index_vr,type_uvr,nlatnlon,
                                missing_dvort.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vort,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,
			       &jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_vort,
                               &iidvw,&ijdvw,a,b,&imdab,&indab,
                               wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvF","shaec",&ier,&jer,&ker,&mer,6,5);
/*
 * Reconstruct the divergent (irrotational) wind components
 * note the argument order idivec(...,v,u,...)
 */
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,&jer);
      NGCALLF(divrtec,DIVRTEC)(&inlat,&inlon,&isym,&one,tmp_vr,tmp_ur,
                                 &iidvw,&ijdvw,a,b,&imdab,&indab,
                                 wvhsec,&ilvhsec,work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvF","vhseci+ivrtec",&ier,&jer,&ker,&mer,
                                 6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ur,tmp_vr,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ur,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uvr == NCL_float) {
        coerce_output_float_only(uvr,tmp_ur,nlatnlon,index_ur);
        coerce_output_float_only(uvr,tmp_vr,nlatnlon,index_vr);
      }
    }
    index_ur = index_vort += nlatnlon;
    index_vr += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_vort != NCL_double) {
    NclFree(tmp_vort);
    NclFree(tmp_ur);
    NclFree(tmp_vr);
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vr2uvF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uvr,ndims_uvr,dsizes_uvr,&missing_uvr,type_uvr,0);
  }
  else {
    ret = NclReturnValue(uvr,ndims_uvr,dsizes_uvr,NULL,type_uvr,0);
  }
  NclFree(dsizes_uvr);
  return(ret);
}


NhlErrorTypes vr2uvg_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing_vort;
/*
 * Output array variables
 */
  void *ur, *vr;
  double *tmp_ur = NULL;
  double *tmp_vr = NULL;
  int ndims_ur;
  ng_size_t dsizes_ur[NCL_MAX_DIMENSIONS];
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int has_missing_ur, has_missing_vr;
  NclScalar missing_ur, missing_vr, missing_dur, missing_dvr;
  NclBasicDataTypes type_ur, type_vr;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_vr;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
/*
 * Get output arrays.
 */
  ur = (void*)NclGetArgValue(
           1,
           3,
           &ndims_ur, 
           dsizes_ur,
           &missing_ur,
           &has_missing_ur,
           &type_ur,
           1);
  vr = (void*)NclGetArgValue(
           2,
           3,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
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
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 NULL);
  coerce_missing(type_ur,has_missing_ur,&missing_ur,&missing_dur,NULL);
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);

/*
 * Allocate space for temporary input array. The temporary array
 * tmp_vort is just big enough to hold a 2-dimensional subsection of the
 * vort array. We only need to allocate space for it if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in vort.
 */
  if(type_vort != NCL_double) {
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vort == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for coercing vort array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_ur != NCL_double) {
    tmp_ur = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_ur == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for coercing ur array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+1)/2 );
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshagc == NULL || wvhsgc == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vr = nmiss = 0;
  
  for(i = 0; i < nt; i++ ) {
    if(type_vort != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vort (tmp_vort) to double.
 */
      coerce_subset_input_double(vort,tmp_vort,index_vr,type_vort,
                                 nlatnlon,0,&missing_vort,&missing_dvort);
    }
    else {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_vr];
    }
    if(type_ur == NCL_double) {
/*
 * Point tmp_ur to appropriate location in ur.
 */
      tmp_ur = &((double*)ur)[index_vr];
    }
    if(type_vr == NCL_double) {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_vr];
    }
/*
 * Check for missing values.
 */
    found_missing_vort = contains_missing(tmp_vort,nlatnlon,has_missing_vort,
                                        missing_dvort.doubleval);
    if(found_missing_vort) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_ur) {
        set_subset_output_missing(ur,index_vr,type_ur,nlatnlon,
                                  missing_dur.doubleval);
      }
      if(has_missing_vr) {
        set_subset_output_missing(vr,index_vr,type_vr,nlatnlon,
                                  missing_dvr.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vort,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,&jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               a,b,&imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvg","shagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Reconstruct the divergent (irrotational) wind components
 * note the argument order idivgc(...,v,u,...)
 */
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,&jer);
      NGCALLF(divrtgc,DIVRTGC)(&inlat,&inlon,&isym,&one,tmp_vr,tmp_ur,
                                 &iidvw,&ijdvw,a,b,&imdab,&indab,
                                 wvhsgc,&ilvhsgc,work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvg","vhsgci+ivrtgc",&ier,&jer,&ker,
                                 &mer,6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ur,tmp_vr,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ur,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_ur == NCL_float) {
        coerce_output_float_only(ur,tmp_ur,nlatnlon,index_vr);
      }
      if(type_vr == NCL_float) {
        coerce_output_float_only(vr,tmp_vr,nlatnlon,index_vr);
      }
    }
    index_vr += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vr2uvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_vort != NCL_double) NclFree(tmp_vort);
  if(type_ur != NCL_double) NclFree(tmp_ur);
  if(type_vr != NCL_double) NclFree(tmp_vr);

  return(NhlNOERROR);
}


NhlErrorTypes vr2uvG_W( void )
{
/*
 * Input array variables
 */
  void *vort;
  double *tmp_vort = NULL;
  int ndims_vort;
  ng_size_t dsizes_vort[NCL_MAX_DIMENSIONS];
  NclScalar missing_vort, missing_dvort, missing_rvort;
  NclBasicDataTypes type_vort;
  int has_missing_vort, found_missing_vort;
/*
 * Output array variables
 */
  void *uvr;
  double *tmp_ur = NULL;
  double *tmp_vr = NULL;
  int ndims_uvr;
  ng_size_t *dsizes_uvr;
  NclScalar missing_uvr;
  NclBasicDataTypes type_uvr;
/*
 * various
 */
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2, l3;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_vr, index_ur, index_vort;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  int ret;
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *a, *b, *pertrb;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vort = (void*)NclGetArgValue(
           0,
           1,
           &ndims_vort, 
           dsizes_vort,
           &missing_vort,
           &has_missing_vort,
           &type_vort,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_vort < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvG: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vort,ndims_vort,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce the missing values.
 */
  coerce_missing(type_vort,has_missing_vort,&missing_vort,&missing_dvort,
                 &missing_rvort);
/*
 * Allocate space for temporary input/output arrays. The temporary arrays
 * are just big enough to hold a 2-dimensional subsection of the
 * full arrays. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have them point
 * to their appropriate locations in vr.
 */
  if(type_vort != NCL_double) {
    type_uvr = NCL_float;
    tmp_ur   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vr   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_vort = (double*)calloc(nlatnlon,sizeof(double));
    uvr      = (void*)calloc(2*total_size_in,sizeof(float));
    if(uvr == NULL || tmp_vort == NULL || tmp_ur == NULL || tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_vort) {
      missing_uvr = missing_rvort;
    }
  } 
  else {
    type_uvr = NCL_double;
    uvr      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uvr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    if(has_missing_vort) {
      missing_uvr = missing_dvort;
    }
  }
/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uvr  = ndims_vort + 1;
  dsizes_uvr = (ng_size_t*)calloc(ndims_uvr,sizeof(ng_size_t));  
  dsizes_uvr[0] = 2;
  for(i = 1; i <= ndims_vort; i++ ) dsizes_uvr[i] = dsizes_vort[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  l3      = max(nlat,(nlon+1)/2 );
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon*nt+max(3*l2,nlon)));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nt*nlon+max(6*l2,nlon)+2*nt*l3+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  a      = (double*)calloc(mdab*ndab,sizeof(double));
  b      = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertrb = (double*)calloc(1,sizeof(double));

  if( work1 == NULL || work2 == NULL || work3 == NULL || 
      dwork1 == NULL || dwork2 == NULL || pertrb == NULL || 
      wshagc == NULL || wvhsgc == NULL || a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vr2uvG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_ur = index_vort = nmiss = 0;
  index_vr = total_size_in;
  
  for(i = 0; i < nt; i++ ) {
    if(type_vort != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vort (tmp_vort) to double.
 */
      coerce_subset_input_double(vort,tmp_vort,index_vort,type_vort,
                                 nlatnlon,0,&missing_vort,&missing_dvort);
    }
    else {
/*
 * Point tmp_vort to appropriate location in vort.
 */
      tmp_vort = &((double*)vort)[index_vort];
    }
    if(type_uvr == NCL_double) {
      tmp_ur = &((double*)uvr)[index_ur];
      tmp_vr = &((double*)uvr)[index_vr];
    }
/*
 * Check for missing values.
 */
    found_missing_vort = contains_missing(tmp_vort,nlatnlon,has_missing_vort,
                                          missing_dvort.doubleval);
    if(found_missing_vort) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(uvr,index_ur,type_uvr,nlatnlon,
                                missing_dvort.doubleval);
      set_subset_output_missing(uvr,index_vr,type_uvr,nlatnlon,
                                missing_dvort.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vort,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian 
 * grid(s) and returns the coefficients in array(s) a,b.
 * Here the scalar grid is "vort" (relative vorticity)
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,&jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_vort,&iidvw,&ijdvw,
                               a,b,&imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,
                               &ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvG","shagc",&ier,&jer,&ker,&mer,6,5);
/*
 * Reconstruct the divergent (irrotational) wind components
 * note the argument order idivgc(...,v,u,...)
 */
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,&jer);
      NGCALLF(divrtgc,DIVRTGC)(&inlat,&inlon,&isym,&one,tmp_vr,tmp_ur,
                                 &iidvw,&ijdvw,a,b,&imdab,&indab,
                                 wvhsgc,&ilvhsgc,work3,&ilwork3,pertrb,&ker);

      NGCALLF(dchkerr,DCHKERR)("vr2uvG","vhsgci+ivrtgc",&ier,&jer,&ker,
                                 &mer,6,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vort,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_ur,tmp_vr,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_ur,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_vr,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uvr == NCL_float) {
        coerce_output_float_only(uvr,tmp_ur,nlatnlon,index_ur);
        coerce_output_float_only(uvr,tmp_vr,nlatnlon,index_vr);
      }
    }
    index_ur = index_vort += nlatnlon;
    index_vr += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertrb);
  NclFree(a);
  NclFree(b);

  if(type_vort != NCL_double) {
    NclFree(tmp_vort);
    NclFree(tmp_ur);
    NclFree(tmp_vr);
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vr2uvG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uvr,ndims_uvr,dsizes_uvr,&missing_uvr,type_uvr,0);
  }
  else {
    ret = NclReturnValue(uvr,ndims_uvr,dsizes_uvr,NULL,type_uvr,0);
  }
  NclFree(dsizes_uvr);
  return(ret);
}


NhlErrorTypes vrdv2uvf_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dvo, missing_dvr, missing_ddvo;
  NclBasicDataTypes type_vr, type_dv;
  int has_missing_vr, has_missing_dvo, found_missing_vr, found_missing_dv;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v;
  NclScalar missing_u, missing_du, missing_v, missing_dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  dv = (void*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dvo,
           &type_dv,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
/*
 * Output arrays must be float or double.
 */ 
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
 * Coerce missing values
 */
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dvo,&missing_dvo,&missing_ddvo,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_dv/tmp_vr are just big enough to hold 2-dimensional subsections
 * of the dv/vr array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv/vr.
 */
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon))+nlat*(4*l1+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  ad     = (double*)calloc(mdab*ndab,sizeof(double));
  bd     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertbd = (double*)calloc(1,sizeof(double));
  pertbv = (double*)calloc(1,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || ad == NULL || bd == NULL || 
     av == NULL || bv == NULL || wshaec == NULL || pertbd == NULL || 
     pertbv == NULL || wvhsec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  
  for(i = 0; i < nt; i++ ) {
    if(type_vr != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vr (tmp_vr) to double.
 */
      coerce_subset_input_double(vr,tmp_vr,index_uv,type_vr,
                                 nlatnlon,0,&missing_vr,&missing_dvr);
    }
    else {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_uv];
    }
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_uv,type_dv,
                                 nlatnlon,0,&missing_dvo,&missing_ddvo);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dvo,
                                        missing_ddvo.doubleval);
    found_missing_vr = contains_missing(tmp_vr,nlatnlon,has_missing_vr,
                                        missing_dvr.doubleval);

    if(found_missing_dv || found_missing_vr) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_dv.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vr,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,&jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,ad,bd,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("vrdv2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from vr,dv
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,&jer);
      NGCALLF(didvtec,DIDVTEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,&iidvw,&ijdvw,
                                 ad,bd,av,bv,&imdab,&indab,wvhsec,&ilvhsec,
                                 work3,&ilwork3,pertbd,pertbv,&ker);

      NGCALLF(dchkerr,DCHKERR)("vrdv2uvf","vhseci+idvtec",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vrdv2uvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertbd);
  NclFree(pertbv);

  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvF_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv, missing_dvr, missing_ddv;
  NclBasicDataTypes type_vr, type_dv;
  int has_missing_vr, has_missing_dv, found_missing_vr, found_missing_dv;
/*
 * Output array variables
 */
  void *uv;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_uv;
  NclScalar missing_uv, missing_duv;
  int ndims_uv;
  ng_size_t *dsizes_uv;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  int ret;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_u, index_v, index_vrdv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (void*)NclGetArgValue(
           0,
           2,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           DONT_CARE);
  dv = (void*)NclGetArgValue(
           1,
           2,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vr,ndims_vr,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce missing values
 */
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_dv/tmp_vr are just big enough to hold 2-dimensional subsections
 * of the dv/vr array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv/vr.
 */
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vr != NCL_double && type_dv != NCL_double) {
    type_uv = NCL_float;
    tmp_u   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_v   = (double*)calloc(nlatnlon,sizeof(double));
    uv      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_u == NULL || tmp_v == NULL || uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_uv = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_duv.doubleval = (double)missing_uv.floatval;
  }
  else {
    type_uv = NCL_double;
    uv      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_duv = missing_uv = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uv  = ndims_vr + 1;
  dsizes_uv = (ng_size_t*)calloc(ndims_uv,sizeof(ng_size_t));  
  dsizes_uv[0] = 2;
  for(i = 1; i <= ndims_vr; i++ ) dsizes_uv[i] = dsizes_vr[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon))+nlat*(4*l1+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  ad     = (double*)calloc(mdab*ndab,sizeof(double));
  bd     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  pertbd = (double*)calloc(1,sizeof(double));
  pertbv = (double*)calloc(1,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || ad == NULL || bd == NULL || 
     av == NULL || bv == NULL || wshaec == NULL || pertbd == NULL || 
     pertbv == NULL || wvhsec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vrdv = index_u = nmiss = 0;
  index_v = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_vr != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vr (tmp_vr) to double.
 */
      coerce_subset_input_double(vr,tmp_vr,index_vrdv,type_vr,
                                 nlatnlon,0,&missing_vr,&missing_dvr);
    }
    else {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_vrdv];
    }
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_vrdv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_vrdv];
    }
    if(type_uv == NCL_double) {
      tmp_u = &((double*)uv)[index_u];
      tmp_v = &((double*)uv)[index_v];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    found_missing_vr = contains_missing(tmp_vr,nlatnlon,has_missing_vr,
                                        missing_dvr.doubleval);

    if(found_missing_dv || found_missing_vr) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(uv,index_u,type_uv,nlatnlon,
                                missing_duv.doubleval);
      set_subset_output_missing(uv,index_v,type_uv,nlatnlon,
                                missing_duv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vr,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,&jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,ad,bd,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("vrdv2uvF","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from vr,dv
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,&jer);
      NGCALLF(didvtec,DIDVTEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,&iidvw,&ijdvw,
                                 ad,bd,av,bv,&imdab,&indab,wvhsec,&ilvhsec,
                                 work3,&ilwork3,pertbd,pertbv,&ker);

      NGCALLF(dchkerr,DCHKERR)("vrdv2uvF","vhseci+idvtec",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uv == NCL_float) {
        coerce_output_float_only(uv,tmp_u,nlatnlon,index_u);
        coerce_output_float_only(uv,tmp_v,nlatnlon,index_v);
      }
    }
    index_u = index_vrdv += nlatnlon;
    index_v += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(wshaec);
  NclFree(wvhsec);
  NclFree(pertbd);
  NclFree(pertbv);

  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_uv != NCL_double) {
    NclFree(tmp_u);
    NclFree(tmp_v);
  }

/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vrdv2uvF: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uv,ndims_uv,dsizes_uv,&missing_uv,type_uv,0);
  }
  else {
    ret = NclReturnValue(uv,ndims_uv,dsizes_uv,NULL,type_uv,0);
  }
  NclFree(dsizes_uv);
  return(ret);
}


NhlErrorTypes vrdv2uvg_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dvo, missing_dvr, missing_ddvo;
  NclBasicDataTypes type_vr, type_dv;
  int has_missing_vr, has_missing_dvo, found_missing_vr, found_missing_dv;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v;
  NclScalar missing_u, missing_du, missing_v, missing_dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ildwork2;
  int ilwork2;
  int ilwork3;
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
           DONT_CARE);
  dv = (void*)NclGetArgValue(
           1,
           4,
           &ndims_dv, 
           dsizes_dv,
           &missing_dvo,
           &has_missing_dvo,
           &type_dv,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvF: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce missing values
 */
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dvo,&missing_dvo,&missing_ddvo,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_dv/tmp_vr are just big enough to hold 2-dimensional subsections
 * of the dv/vr array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv/vr.
 */
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon)+4*l1+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  ad     = (double*)calloc(mdab*ndab,sizeof(double));
  bd     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertbd = (double*)calloc(1,sizeof(double));
  pertbv = (double*)calloc(1,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || ad == NULL || bd == NULL || 
     av == NULL || bv == NULL || wshagc == NULL || pertbd == NULL || 
     pertbv == NULL || wvhsgc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (ldwork2 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;

  for(i = 0; i < nt; i++ ) {
    if(type_vr != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vr (tmp_vr) to double.
 */
      coerce_subset_input_double(vr,tmp_vr,index_uv,type_vr,
                                 nlatnlon,0,&missing_vr,&missing_dvr);
    }
    else {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_uv];
    }
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_uv,type_dv,
                                 nlatnlon,0,&missing_dvo,&missing_ddvo);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dvo,
                                        missing_ddvo.doubleval);
    found_missing_vr = contains_missing(tmp_vr,nlatnlon,has_missing_vr,
                                        missing_dvr.doubleval);

    if(found_missing_dv || found_missing_vr) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_dv.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vr,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,&jer); 
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,ad,bd,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
    
      NGCALLF(dchkerr,DCHKERR)("vrdv2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from vr,dv
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,&jer);
      NGCALLF(didvtgc,DIDVTGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,&iidvw,&ijdvw,
                                 ad,bd,av,bv,&imdab,&indab,wvhsgc,&ilvhsgc,
                                 work3,&ilwork3,pertbd,pertbv,&ker);

      NGCALLF(dchkerr,DCHKERR)("vrdv2uvg","vhsgci+idvtgc",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vrdv2uvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertbd);
  NclFree(pertbv);

  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes vrdv2uvG_W( void )
{
/*
 * Input array variables
 */
  void *vr, *dv;
  double *tmp_vr = NULL;
  double *tmp_dv = NULL;
  int ndims_vr;
  ng_size_t dsizes_vr[NCL_MAX_DIMENSIONS];
  int ndims_dv;
  ng_size_t dsizes_dv[NCL_MAX_DIMENSIONS];
  NclScalar missing_vr, missing_dv, missing_dvr, missing_ddv;
  NclBasicDataTypes type_vr, type_dv;
  int has_missing_vr, has_missing_dv, found_missing_vr, found_missing_dv;
/*
 * Output array variables
 */
  void *uv;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_uv;
  NclScalar missing_uv, missing_duv;
  int ndims_uv;
  ng_size_t *dsizes_uv;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  int ret;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_u, index_v, index_vrdv;
  int nmiss;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *ad, *bd, *av, *bv, *pertbd, *pertbv;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ilwork2;
  int ilwork3;
  int ildwork2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  vr = (void*)NclGetArgValue(
           0,
           2,
           &ndims_vr, 
           dsizes_vr,
           &missing_vr,
           &has_missing_vr,
           &type_vr,
           DONT_CARE);
  dv = (void*)NclGetArgValue(
           1,
           2,
           &ndims_dv, 
           dsizes_dv,
           &missing_dv,
           &has_missing_dv,
           &type_dv,
           DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_vr != ndims_dv || ndims_vr < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_vr; i++ ) {
    if( dsizes_vr[i] != dsizes_dv[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_vr,ndims_vr,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce missing values
 */
  coerce_missing(type_vr,has_missing_vr,&missing_vr,&missing_dvr,NULL);
  coerce_missing(type_dv,has_missing_dv,&missing_dv,&missing_ddv,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_dv/tmp_vr are just big enough to hold 2-dimensional subsections
 * of the dv/vr array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in dv/vr.
 */
  if(type_vr != NCL_double) {
    tmp_vr = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: Unable to allocate memory for coercing vr array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_dv != NCL_double) {
    tmp_dv = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_dv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: Unable to allocate memory for coercing dv array to double precision");
      return(NhlFATAL);
    }
  } 
  if(type_vr != NCL_double && type_dv != NCL_double) {
    type_uv = NCL_float;
    tmp_u   = (double*)calloc(nlatnlon,sizeof(double));
    tmp_v   = (double*)calloc(nlatnlon,sizeof(double));
    uv      = (void*)calloc(2*total_size_in,sizeof(float));
    if(tmp_u == NULL || tmp_v == NULL || uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: Unable to allocate memory for temporary arrays");
      return(NhlFATAL);
    }
    missing_uv = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
    missing_duv.doubleval = (double)missing_uv.floatval;
  }
  else {
    type_uv = NCL_double;
    uv      = (void*)calloc(2*total_size_in,sizeof(double));
    if(uv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    missing_duv = missing_uv = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
  } 

/*
 * Figure out dimensions of output array. It will be dimensioned
 * 2 x nt x nlat x nlon, where the 0th dimension of the first
 * dimension represents ud, and the 1th dimension represents vd.
 */
  ndims_uv  = ndims_vr + 1;
  dsizes_uv = (ng_size_t*)calloc(ndims_uv,sizeof(ng_size_t));  
  dsizes_uv[0] = 2;
  for(i = 1; i <= ndims_vr; i++ ) dsizes_uv[i] = dsizes_vr[i-1];

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon)+4*l1+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  ad     = (double*)calloc(mdab*ndab,sizeof(double));
  bd     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  pertbd = (double*)calloc(1,sizeof(double));
  pertbv = (double*)calloc(1,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || ad == NULL || bd == NULL || 
     av == NULL || bv == NULL || wshagc == NULL || pertbd == NULL || 
     pertbv == NULL || wvhsgc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX) ||
     (ldwork2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vrdv2uvG: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_vrdv = index_u = nmiss = 0;
  index_v = total_size_in;

  for(i = 0; i < nt; i++ ) {
    if(type_vr != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vr (tmp_vr) to double.
 */
      coerce_subset_input_double(vr,tmp_vr,index_vrdv,type_vr,
                                 nlatnlon,0,&missing_vr,&missing_dvr);
    }
    else {
/*
 * Point tmp_vr to appropriate location in vr.
 */
      tmp_vr = &((double*)vr)[index_vrdv];
    }
    if(type_dv != NCL_double) {
/*
 * Coerce nlat x nlon subsection of dv (tmp_dv) to double.
 */
      coerce_subset_input_double(dv,tmp_dv,index_vrdv,type_dv,
                                 nlatnlon,0,&missing_dv,&missing_ddv);
    }
    else {
/*
 * Point tmp_dv to appropriate location in dv.
 */
      tmp_dv = &((double*)dv)[index_vrdv];
    }
    if(type_uv == NCL_double) {
      tmp_u = &((double*)uv)[index_u];
      tmp_v = &((double*)uv)[index_v];
    }
/*
 * Check for missing values.
 */
    found_missing_dv = contains_missing(tmp_dv,nlatnlon,has_missing_dv,
                                        missing_ddv.doubleval);
    found_missing_vr = contains_missing(tmp_vr,nlatnlon,has_missing_vr,
                                        missing_dvr.doubleval);

    if(found_missing_dv || found_missing_vr) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      set_subset_output_missing(uv,index_u,type_uv,nlatnlon,
                                missing_duv.doubleval);
      set_subset_output_missing(uv,index_v,type_uv,nlatnlon,
                                missing_duv.doubleval);
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_dv,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vr,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) ad,bd for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,&jer);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_dv,&iidvw,&ijdvw,ad,bd,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_vr,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      NGCALLF(dchkerr,DCHKERR)("vrdv2uvG","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from vr,dv
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,&jer);
      NGCALLF(didvtgc,DIDVTGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,&iidvw,&ijdvw,
                                 ad,bd,av,bv,&imdab,&indab,wvhsgc,&ilvhsgc,
                                 work3,&ilwork3,pertbd,pertbv,&ker);
      NGCALLF(dchkerr,DCHKERR)("vrdv2uvG","vhsgci+idvtgc",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_dv,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vr,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&scale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&scale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_uv == NCL_float) {
        coerce_output_float_only(uv,tmp_u,nlatnlon,index_u);
        coerce_output_float_only(uv,tmp_v,nlatnlon,index_v);
      }
    }
    index_u = index_vrdv += nlatnlon;
    index_v += nlatnlon;
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(ad);
  NclFree(bd);
  NclFree(av);
  NclFree(bv);
  NclFree(wshagc);
  NclFree(wvhsgc);
  NclFree(pertbd);
  NclFree(pertbv);

  if(type_vr != NCL_double) NclFree(tmp_vr);
  if(type_dv != NCL_double) NclFree(tmp_dv);
  if(type_uv != NCL_double) {
    NclFree(tmp_u);
    NclFree(tmp_v);
  }

/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"vrdv2uvG: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
    ret = NclReturnValue(uv,ndims_uv,dsizes_uv,&missing_uv,type_uv,0);
  }
  else {
    ret = NclReturnValue(uv,ndims_uv,dsizes_uv,NULL,type_uv,0);
  }
  NclFree(dsizes_uv);
  return(ret);
}


NhlErrorTypes sfvp2uvf_W( void )
{
/*
 * Input array variables
 */
  void *sf, *vp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  int ndims_sf;
  ng_size_t dsizes_sf[NCL_MAX_DIMENSIONS];
  int ndims_vp;
  ng_size_t dsizes_vp[NCL_MAX_DIMENSIONS];
  NclScalar missing_sf, missing_vpo, missing_dsf, missing_dvpo;
  NclBasicDataTypes type_sf, type_vp;
  int has_missing_sf, has_missing_vpo, found_missing_sf, found_missing_vp;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v;
  NclScalar missing_u, missing_du, missing_v, missing_vp;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshaec, lvhsec;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshaec, *wvhsec, *as, *bs, *av, *bv;
  int inlon;
  int inlat;
  int ilshaec;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ilwork3;
  int ilwork2;
  int ildwork2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  sf = (void*)NclGetArgValue(
           0,
           4,
           &ndims_sf, 
           dsizes_sf,
           &missing_sf,
           &has_missing_sf,
           &type_sf,
           DONT_CARE);
  vp = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vp, 
           dsizes_vp,
           &missing_vpo,
           &has_missing_vpo,
           &type_vp,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_sf != ndims_vp || ndims_sf < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_sf; i++ ) {
    if( dsizes_sf[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_sf || ndims_v != ndims_sf ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_sf; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_v[i] != dsizes_sf[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce missing values
 */
  coerce_missing(type_sf,has_missing_sf,&missing_sf,&missing_dsf,NULL);
  coerce_missing(type_vp,has_missing_vpo,&missing_vpo,&missing_dvpo,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_vp,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_vp/tmp_sf are just big enough to hold 2-dimensional subsections
 * of the vp/sf array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in vp/sf.
 */
  if(type_sf != NCL_double) {
    tmp_sf = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_sf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: Unable to allocate memory for coercing sf array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vp != NCL_double) {
    tmp_vp = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: Unable to allocate memory for coercing vp array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(2*(nlat+1),nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*(nlat+1),nlat*(2*nlon+max(6*l2,nlon))+nlat*(4*l1+1));
  ldwork1 = nlat+1;
  ldwork2 = 2*(nlat+2);
  lvhsec  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1+1)+nlon+15;
  lshaec  = 2*nlat*l2+3*(max(l1-2,0)*(nlat+nlat-l1-1))/2+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  as     = (double*)calloc(mdab*ndab,sizeof(double));
  bs     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshaec = (double*)calloc(lshaec,sizeof(double));
  wvhsec = (double*)calloc(lvhsec,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || as == NULL || bs == NULL || 
     av == NULL || bv == NULL || wshaec == NULL || wvhsec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (lwork3 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (ldwork2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvf: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshaec = (int) lshaec;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_sf != NCL_double) {
/*
 * Coerce nlat x nlon subsection of sf (tmp_sf) to double.
 */
      coerce_subset_input_double(sf,tmp_sf,index_uv,type_sf,
                                 nlatnlon,0,&missing_sf,&missing_dsf);
    }
    else {
/*
 * Point tmp_sf to appropriate location in sf.
 */
      tmp_sf = &((double*)sf)[index_uv];
    }
    if(type_vp != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vp (tmp_vp) to double.
 */
      coerce_subset_input_double(vp,tmp_vp,index_uv,type_vp,
                                 nlatnlon,0,&missing_vpo,&missing_dvpo);
    }
    else {
/*
 * Point tmp_vp to appropriate location in vp.
 */
      tmp_vp = &((double*)vp)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_vp = contains_missing(tmp_vp,nlatnlon,has_missing_vpo,
                                        missing_dvpo.doubleval);
    found_missing_sf = contains_missing(tmp_sf,nlatnlon,has_missing_sf,
                                        missing_dsf.doubleval);

    if(found_missing_vp || found_missing_sf) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_vp.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vp,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_sf,work1);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) as,bs for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork1,&ildwork1,&jer);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_vp,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);
      NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&one,tmp_sf,&iidvw,&ijdvw,as,bs,
                               &imdab,&indab,wshaec,&ilshaec,work2,&ilwork2,&ker);

      NGCALLF(dchkerr,DCHKERR)("sfvp2uvf","shaec",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from sf,vp
 */ 
      NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork2,&ildwork2,&jer);
      NGCALLF(disfvpec,DISFVPEC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                                   &iidvw,&ijdvw,as,bs,av,bv,&imdab,&indab,
                                   wvhsec,&ilvhsec,work3,&ilwork3,&ker);
      NGCALLF(dchkerr,DCHKERR)("sfvp2uvf","isfvpec",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"sfvp2uvf: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(as);
  NclFree(bs);
  NclFree(av);
  NclFree(bv);
  NclFree(wshaec);
  NclFree(wvhsec);

  if(type_sf != NCL_double) NclFree(tmp_sf);
  if(type_vp != NCL_double) NclFree(tmp_vp);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes sfvp2uvg_W( void )
{
/*
 * Input array variables
 */
  void *sf, *vp;
  double *tmp_sf = NULL;
  double *tmp_vp = NULL;
  int ndims_sf;
  ng_size_t dsizes_sf[NCL_MAX_DIMENSIONS];
  int ndims_vp;
  ng_size_t dsizes_vp[NCL_MAX_DIMENSIONS];
  NclScalar missing_sf, missing_vpo, missing_dsf, missing_dvpo;
  NclBasicDataTypes type_sf, type_vp;
  int has_missing_sf, has_missing_vpo, found_missing_sf, found_missing_vp;
/*
 * Output array variables
 */
  void *u, *v;
  double *tmp_u = NULL;
  double *tmp_v = NULL;
  NclBasicDataTypes type_u, type_v;
  int has_missing_u, has_missing_v;
  NclScalar missing_u, missing_du, missing_v, missing_vp;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in;
  ng_size_t i, idvw, jdvw, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0, ner=0, one=1;
  ng_size_t index_uv;
  int nmiss;
  double invscale;
/*
 * Workspace variables
 */
  ng_size_t lwork1, lwork2, lwork3, ldwork1, ldwork2, lshagc, lvhsgc;
  double *work1, *work2, *work3, *dwork1, *dwork2;
  double *wshagc, *wvhsgc, *as, *bs, *av, *bv;
  int inlon;
  int inlat;
  int ilshagc;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork1;
  int ilwork2;
  int ilwork3;
  int ildwork2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  sf = (void*)NclGetArgValue(
           0,
           4,
           &ndims_sf, 
           dsizes_sf,
           &missing_sf,
           &has_missing_sf,
           &type_sf,
           DONT_CARE);
  vp = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vp, 
           dsizes_vp,
           &missing_vpo,
           &has_missing_vpo,
           &type_vp,
           DONT_CARE);
/*
 * Get output arrays.
 */
  u = (void*)NclGetArgValue(
           2,
           4,
           &ndims_u, 
           dsizes_u,
           &missing_u,
           &has_missing_u,
           &type_u,
           1);
  v = (void*)NclGetArgValue(
           3,
           4,
           &ndims_v, 
           dsizes_v,
           &missing_v,
           &has_missing_v,
           &type_v,
           1);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_sf != ndims_vp || ndims_sf < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_sf; i++ ) {
    if( dsizes_sf[i] != dsizes_vp[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The output arrays must also be at least 2-dimensional and the same sizes.
 */
  if( ndims_u != ndims_sf || ndims_v != ndims_sf ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_sf; i++ ) {
    if( dsizes_u[i] != dsizes_sf[i] || dsizes_v[i] != dsizes_sf[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: The input/output arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
  if((type_u != NCL_float && type_u != NCL_double) ||
     (type_v != NCL_float && type_v != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
/*
 * Coerce missing values
 */
  coerce_missing(type_sf,has_missing_sf,&missing_sf,&missing_dsf,NULL);
  coerce_missing(type_vp,has_missing_vpo,&missing_vpo,&missing_dvpo,NULL);
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,NULL);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_vp,NULL);

/*
 * Allocate space for temporary input arrays. The temporary arrays
 * tmp_vp/tmp_sf are just big enough to hold 2-dimensional subsections
 * of the vp/sf array. We only need to allocate space for them if the
 * input is not already double. Otherwise, we just have it point
 * to the appropriate locations in vp/sf.
 */
  if(type_sf != NCL_double) {
    tmp_sf = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_sf == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: Unable to allocate memory for coercing sf array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_vp != NCL_double) {
    tmp_vp = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_vp == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: Unable to allocate memory for coercing vp array to double precision");
      return(NhlFATAL);
    }
  } 
/*
 * Allocate space for temporary output arrays, if not already double.
 */
  if(type_u != NCL_double) {
    tmp_u = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_u == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: Unable to allocate memory for coercing u array to double precision");
      return(NhlFATAL);
    }
  } 

  if(type_v != NCL_double) {
    tmp_v = (double*)calloc(nlatnlon,sizeof(double));
    if(tmp_v == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: Unable to allocate memory for coercing v array to double precision");
      return(NhlFATAL);
    }
  } 

/*
 * Allocate memory for work arrays.
 */
  isym    = 0;
  idvw    = nlat;
  jdvw    = nlon;
  ndab    = nlat;
  mdab    = min(nlat,(nlon+2)/2);
  l1      = min(nlat,(nlon+2)/2);
  l2      = (nlat+1)/2;
  lwork1  = nlatnlon;
  lwork2  = max(4*nlat*(nlat+2)+2,nlat*(nlon+max(3*l2,nlon)));
  lwork3  = max(4*nlat*(nlat+1)+2,nlat*(2*nlon+max(6*l2,nlon)+4*l1+1));
  ldwork1 = nlat*(nlat+4);
  ldwork2 = 2*nlat*(nlat+1)+1;
  lshagc  = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+nlon+15;
  lvhsgc  = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1+1)+nlon+15;

  work1  = (double*)calloc(lwork1,sizeof(double));
  work2  = (double*)calloc(lwork2,sizeof(double));
  work3  = (double*)calloc(lwork3,sizeof(double));
  dwork1 = (double*)calloc(ldwork1,sizeof(double));
  dwork2 = (double*)calloc(ldwork2,sizeof(double));
  as     = (double*)calloc(mdab*ndab,sizeof(double));
  bs     = (double*)calloc(mdab*ndab,sizeof(double));
  av     = (double*)calloc(mdab*ndab,sizeof(double));
  bv     = (double*)calloc(mdab*ndab,sizeof(double));
  wshagc = (double*)calloc(lshagc,sizeof(double));
  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));

  if(work1 == NULL || work2 == NULL || work3 == NULL || 
     dwork1 == NULL || dwork2 == NULL || as == NULL || bs == NULL || 
     av == NULL || bv == NULL || wshagc == NULL || wvhsgc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork1 > INT_MAX) ||
     (lwork2 > INT_MAX) ||
     (lwork3 > INT_MAX) ||
     (ldwork2 > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"sfvp2uvg: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshagc = (int) lshagc;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ildwork1 = (int) ldwork1;
  ildwork2 = (int) ldwork2;
  ilwork2 = (int) lwork2;
  ilwork3 = (int) lwork3;
/*
 * Loop through the rightmost nt dimensions and call the various
 * underlying Fortran routines. This code could also be written
 * so that the full input arrays are passed to the Fortran routines,
 * but then you can't check each individual nlat x nlon array to see
 * if contains missing values.  In the case below, if an nlat x nlon
 * array contains missing values, then only that subsection is set to
 * all missing.
 */
  index_uv = nmiss = 0;
  invscale = 1./scale;       /* 1/(radius of earth) */

  for(i = 0; i < nt; i++ ) {
    if(type_sf != NCL_double) {
/*
 * Coerce nlat x nlon subsection of sf (tmp_sf) to double.
 */
      coerce_subset_input_double(sf,tmp_sf,index_uv,type_sf,
                                 nlatnlon,0,&missing_sf,&missing_dsf);
    }
    else {
/*
 * Point tmp_sf to appropriate location in sf.
 */
      tmp_sf = &((double*)sf)[index_uv];
    }
    if(type_vp != NCL_double) {
/*
 * Coerce nlat x nlon subsection of vp (tmp_vp) to double.
 */
      coerce_subset_input_double(vp,tmp_vp,index_uv,type_vp,
                                 nlatnlon,0,&missing_vpo,&missing_dvpo);
    }
    else {
/*
 * Point tmp_vp to appropriate location in vp.
 */
      tmp_vp = &((double*)vp)[index_uv];
    }
    if(type_u == NCL_double) {
/*
 * Point tmp_u to appropriate location in u.
 */
      tmp_u = &((double*)u)[index_uv];
    }
    if(type_v == NCL_double) {
/*
 * Point tmp_v to appropriate location in v.
 */
      tmp_v = &((double*)v)[index_uv];
    }
/*
 * Check for missing values.
 */
    found_missing_vp = contains_missing(tmp_vp,nlatnlon,has_missing_vpo,
                                        missing_dvpo.doubleval);
    found_missing_sf = contains_missing(tmp_sf,nlatnlon,has_missing_sf,
                                        missing_dsf.doubleval);

    if(found_missing_vp || found_missing_sf) {
      nmiss++;
/*
 * Set all elements of these 2D grids to a missing value, if a missing
 * value exists.
 */
      if(has_missing_u) {
        set_subset_output_missing(u,index_uv,type_u,nlatnlon,
                                  missing_du.doubleval);
      }
      if(has_missing_v) {
        set_subset_output_missing(v,index_uv,type_v,nlatnlon,
                                  missing_vp.doubleval);
      }
    }
    else {

/*
 * Transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_vp,work1);
      NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,tmp_sf,work1);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) as,bs for divergence
 * and av,bv for vortivity
 */
      NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork1,&ildwork1,&jer); 
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_vp,&iidvw,&ijdvw,av,bv,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&one,tmp_sf,&iidvw,&ijdvw,as,bs,
                               &imdab,&indab,wshagc,&ilshagc,work2,&ilwork2,&ker);
      
      NGCALLF(dchkerr,DCHKERR)("sfvp2uvg","shagc",&ier,&jer,&ker,&mer,8,5);
/* 
 * Compute the u and v components from sf, vp
 */ 
      NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork2,&ildwork2,&jer);
      NGCALLF(disfvpgc,DISFVPGC)(&inlat,&inlon,&isym,&one,tmp_v,tmp_u,
                                   &iidvw,&ijdvw,as,bs,av,bv,&imdab,&indab,
                                   wvhsgc,&ilvhsgc,work3,&ilwork3,&ker);
      NGCALLF(dchkerr,DCHKERR)("sfvp2uvg","isfvpgc",&ier,&jer,&ker,
                                 &mer,8,13);
/* 
 * Transform from math coordinates to geophysical coordinates.
 * (math) nlon is the last dim
 */
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_vp,work1);
      NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,tmp_sf,work1);
      NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,tmp_u,tmp_v,work1);
/*
 * (Possibly) scale the quantities calculated by this routine
 */
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_u,&invscale,&ner);
      NGCALLF(dgeoscl,DGEOSCL)(&inlon,&inlat,&one,tmp_v,&invscale,&ner);

/*
 * Coerce output back to float if necessary.
 */
      if(type_u == NCL_float) {
        coerce_output_float_only(u,tmp_u,nlatnlon,index_uv);
      }
      if(type_v == NCL_float) {
        coerce_output_float_only(v,tmp_v,nlatnlon,index_uv);
      }
    }
    index_uv += nlatnlon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"sfvp2uvg: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",nmiss);
  }
/*
 * Free the work arrays.
 */
  NclFree(work1);
  NclFree(work2);
  NclFree(work3);
  NclFree(dwork1);
  NclFree(dwork2);
  NclFree(as);
  NclFree(bs);
  NclFree(av);
  NclFree(bv);
  NclFree(wshagc);
  NclFree(wvhsgc);

  if(type_sf != NCL_double) NclFree(tmp_sf);
  if(type_vp != NCL_double) NclFree(tmp_vp);
  if(type_u != NCL_double) NclFree(tmp_u);
  if(type_v != NCL_double) NclFree(tmp_v);

  return(NhlNOERROR);
}


NhlErrorTypes vhaec_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void    *br,  *bi,  *cr,  *ci;
  double *dbr, *dbi, *dcr, *dci;
  float  *rbr, *rbi, *rcr, *rci;
  int ndims_br;
  ng_size_t dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi;
  ng_size_t dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr;
  ng_size_t dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci;
  ng_size_t dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhaec;
  double *work, *wvhaec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhaec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           DONT_CARE);
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
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
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
/*
 * Output arrays must be float or double.
 */
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
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL);
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
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,&du[j],&dv[j],work);
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

/*
 * Test dimension sizes
 */
  if((nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilvhaec = (int) lvhaec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork,&ildwork,&jer);
  NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
                             &iidvw,&ijdvw,dbr,dbi,dcr,dci,
                             &imdab,&indab,wvhaec,&ilvhaec,work,&ilwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhaec","vhaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
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


NhlErrorTypes vhaeC_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  double *dbc;
  float  *rbc;
  int ndims_bc;
  ng_size_t dsizes_bc[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhaec;
  double *work, *wvhaec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhaec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           NULL,
           NULL,
           &type_u,
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_out = 4 * nt * nlat * nlat;
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  dbc = (double*)calloc(total_size_out,sizeof(double));
  if( dbc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,&du[j],&dv[j],work);
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

  if((nt > INT_MAX) ||
     (lvhaec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilvhaec = (int) lvhaec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wvhaec = (double*)calloc(lvhaec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhaeci,DVHAECI)(&inlat,&inlon,wvhaec,&ilvhaec,dwork,&ildwork,&jer);
  j = nt * nlat * nlat;
  NGCALLF(dvhaec,DVHAEC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
			 &iidvw,&ijdvw,&dbc[0],&dbc[j],&dbc[2*j],&dbc[3*j],
			 &imdab,&indab,wvhaec,&ilvhaec,work,&ilwork,&ker);
  NclFree(wvhaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhaec","vhaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

/*
 * Return
 */
  ndims_bc = ndims_u + 1;
  dsizes_bc[0] = 4;
  for( i = 1; i < ndims_bc-2; i++ ) dsizes_bc[i] = dsizes_u[i-1];
  dsizes_bc[ndims_bc-1] = dsizes_bc[ndims_bc-2] = nlat;
/*
 * Determine whether to return float or double.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    rbc = (float*)calloc(total_size_out,sizeof(float));
    if (rbc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhaeC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rbc[i] = (float)dbc[i];
/*
 * Free double precision array.
 */
    NclFree(dbc);

    return(NclReturnValue((void*)rbc,ndims_bc,dsizes_bc,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dbc,ndims_bc,dsizes_bc,NULL,NCL_double,0));
  }
}


NhlErrorTypes vhagc_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  void    *br,  *bi,  *cr,  *ci;
  double *dbr, *dbi, *dcr, *dci;
  float  *rbr, *rbi, *rcr, *rci;
  int ndims_br;
  ng_size_t dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi;
  ng_size_t dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr;
  ng_size_t dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci;
  ng_size_t dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhagc;
  double *work, *wvhagc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhagc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           6,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           DONT_CARE);
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
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
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
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL);
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
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes
 */
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,&du[j],&dv[j],work);
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


/*
 * Test dimension sizes.
 */
  if((nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilvhagc = (int) lvhagc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wvhagc = (double*)calloc( lvhagc,sizeof(double));
  work   = (double*)calloc(  lwork,sizeof(double));
  dwork  = (double*)calloc( ldwork,sizeof(double));

  if( wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork,&ildwork,&jer);
  NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
                             &iidvw,&ijdvw,dbr,dbi,dcr,dci,
                             &imdab,&indab,wvhagc,&ilvhagc,work,&ilwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhagc","vhagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
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

NhlErrorTypes vhagC_W( void )
{
/*
 * Input array variables
 */
  void *u, *v;
  double *du, *dv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * Output array variables
 */
  double *dbc;
  float  *rbc;
  int ndims_bc;
  ng_size_t dsizes_bc[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhagc;
  double *work, *wvhagc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhagc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           NULL,
           NULL,
           &type_u,
           DONT_CARE);
  v = (void*)NclGetArgValue(
           1,
           2,
           &ndims_v, 
           dsizes_v,
           NULL,
           NULL,
           &type_v,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if( ndims_u != ndims_v || ndims_u < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_u; i++ ) {
    if( dsizes_u[i] != dsizes_v[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: The input arrays must have the same dimension sizes");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatnlon(dsizes_u,ndims_u,&nlat,&nlon,&nlatnlon,&nt,
                   &total_size_in);
  total_size_out = 4 * nt * nlat * nlat;
/*
 * Coerce u and v.
 */
  du = coerce_input_double(u,type_u,total_size_in,0,NULL,NULL);
  dv = coerce_input_double(v,type_v,total_size_in,0,NULL,NULL);
  if(du == NULL || dv == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  dbc  = (double*)calloc(total_size_out,sizeof(double));
  if( dbc == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes
 */
  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomatv,DGEOMATV)(&inlon,&inlat,&du[j],&dv[j],work);
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

/*
 * Test dimension sizes
 */
  if((nt > INT_MAX) ||
     (lvhagc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilvhagc = (int) lvhagc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;


  wvhagc = (double*)calloc( lvhagc,sizeof(double));
  work   = (double*)calloc(  lwork,sizeof(double));
  dwork  = (double*)calloc( ldwork,sizeof(double));

  if( wvhagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhagci,DVHAGCI)(&inlat,&inlon,wvhagc,&ilvhagc,dwork,&ildwork,&jer);
  j = nt * nlat * nlat;
  NGCALLF(dvhagc,DVHAGC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
			 &iidvw,&ijdvw,&dbc[0],&dbc[j],&dbc[2*j],&dbc[3*j],
			 &imdab,&indab,wvhagc,&ilvhagc,work,&ilwork,&ker);
  NclFree(wvhagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhagC","vhagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);
  if((void*)du != u) NclFree(du);
  if((void*)dv != v) NclFree(dv);

/*
 * Return
 */
  ndims_bc = ndims_u + 1;
  dsizes_bc[0] = 4;
  for( i = 1; i < ndims_bc-2; i++ ) dsizes_bc[i] = dsizes_u[i-1];
  dsizes_bc[ndims_bc-1] = dsizes_bc[ndims_bc-2] = nlat;
/*
 * Determine whether to return float or double.
 */
  if(type_u != NCL_double && type_v != NCL_double) {
    rbc = (float*)calloc(total_size_out,sizeof(float));
    if (rbc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhagC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) rbc[i] = (float)dbc[i];
/*
 * Free double precision array.
 */
    NclFree(dbc);

    return(NclReturnValue((void*)rbc,ndims_bc,dsizes_bc,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)dbc,ndims_bc,dsizes_bc,NULL,NCL_double,0));
  }
}

NhlErrorTypes vhsec_W( void )
{
/*
 * Input array variables
 */
  void *br, *bi, *cr, *ci;
  double *dbr, *dbi, *dcr, *dci;
  int ndims_br;
  ng_size_t dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi;
  ng_size_t dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr;
  ng_size_t dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci;
  ng_size_t dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * Output array variables
 */
  void    *u,  *v;
  double *du, *dv;
  float  *ru, *rv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhsec;
  double *work, *wvhsec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
  bi = (void*)NclGetArgValue(
           1,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           DONT_CARE);
  cr = (void*)NclGetArgValue(
           2,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           DONT_CARE);
  ci = (void*)NclGetArgValue(
           3,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           DONT_CARE);
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
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: The input/output arrays must have the same leftmost dimension sizes");
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
  dbr = coerce_input_double(br,type_br,total_size_in,0,NULL,NULL);
  dbi = coerce_input_double(bi,type_bi,total_size_in,0,NULL,NULL);
  dcr = coerce_input_double(cr,type_cr,total_size_in,0,NULL,NULL);
  dci = coerce_input_double(ci,type_ci,total_size_in,0,NULL,NULL);
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

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork,&ildwork,&jer);
  NGCALLF(dvhsec,DVHSEC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
			 &iidvw,&ijdvw,dbr,dbi,dcr,dci,&imdab,&indab,
			 wvhsec,&ilvhsec,work,&ilwork,&ker);

  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhsec","vhsec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
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


NhlErrorTypes vhseC_W( void )
{
/*
 * Input array variables
 */
  void *bc;
  double *dbc;
  int ndims_bc;
  ng_size_t dsizes_bc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_bc, type_nlon;
/*
 * Output array variables
 */
  double *duv;
  float  *ruv;
  int ndims_uv;
  ng_size_t dsizes_uv[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, ntnlatnlon, ntnlatnlat;
  ng_size_t total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
  ng_size_t *nlon_dims;
  void *nlon_in;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhsec;
  double *work, *wvhsec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhsec;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  bc = (void*)NclGetArgValue(
           0,
           2,
           &ndims_bc,
           dsizes_bc,
           NULL,
           NULL,
           &type_bc,
           DONT_CARE);
  nlon_in = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_nlon,
           DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  nlon_dims = get_dimensions(nlon_in,1,type_nlon,"vhseC");
  if(nlon_dims == NULL) 
    return(NhlFATAL);
  nlon = *nlon_dims;
  NclFree(nlon_dims);

/*
 * The grid coming in must be at least 3-dimensional.
 */
  if( ndims_bc < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_bc[0] != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: The first dimension of the input array must be 4");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  nlat = dsizes_bc[ndims_bc-1];
  nlatnlon = nlon * nlat; 

  nt = 1;
  for(i = 1; i < ndims_bc-2; i++) nt *= dsizes_bc[i];
  ntnlatnlon = nt * nlatnlon;
  ntnlatnlat = nt * nlat * nlat;

  total_size_out = 2 * ntnlatnlon;
  total_size_in  = 4 * ntnlatnlat;
/*
 * Coerce bc.
 */
  dbc = coerce_input_double(bc,type_bc,total_size_in,0,NULL,NULL);
  if(dbc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  duv  = (double*)calloc(total_size_out,sizeof(double));
  if( duv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: Unable to allocate memory for uv array");
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

  lwork  = max(4*(nlat+1),nlat*(2*nt* nlon +max(6*l2,nlon)));
  lvhsec = 4*nlat*l2+3*max(l1-2,0)*(2*nlat-l1-1)+ nlon+15;
  ldwork = 2*(nlat+2);

  wvhsec = (double*)calloc(lvhsec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhsec > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilvhsec = (int) lvhsec;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  NGCALLF(dvhseci,DVHSECI)(&inlat,&inlon,wvhsec,&ilvhsec,dwork,&ildwork,&jer);
  i = ntnlatnlat;
  j = ntnlatnlon;
  NGCALLF(dvhsec,DVHSEC)(&inlat,&inlon,&ityp,&i_nt,&duv[j],&duv[0],
			 &iidvw,&ijdvw,&dbc[0],&dbc[i],&dbc[2*i],&dbc[3*i],
			 &imdab,&indab,wvhsec,&ilvhsec,work,&ilwork,&ker);

  NclFree(wvhsec);
  NclFree(work);
  NclFree(dwork);
  if(type_bc != NCL_double) NclFree(dbc);

  NGCALLF(dchkerr,DCHKERR)("vhseC","vhsec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&duv[j],&duv[j+ntnlatnlon],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);

/*
 * Return
 */
  ndims_uv = ndims_bc;
  dsizes_uv[0] = 2;
  for( i = 1; i < ndims_uv-2; i++ ) dsizes_uv[i] = dsizes_bc[i];
  dsizes_uv[ndims_uv-2] = nlat;
  dsizes_uv[ndims_uv-1] = nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_bc != NCL_double) {
    ruv = (float*)calloc(total_size_out,sizeof(float));
    if (ruv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhseC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) ruv[i] = (float)duv[i];
/*
 * Free double precision array.
 */
    NclFree(duv);

    return(NclReturnValue((void*)ruv,ndims_uv,dsizes_uv,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)duv,ndims_uv,dsizes_uv,NULL,NCL_double,0));
  }
}


NhlErrorTypes vhsgc_W( void )
{
/*
 * Input array variables
 */
  void *br, *bi, *cr, *ci;
  double *dbr, *dbi, *dcr, *dci;
  int ndims_br;
  ng_size_t dsizes_br[NCL_MAX_DIMENSIONS];
  int ndims_bi;
  ng_size_t dsizes_bi[NCL_MAX_DIMENSIONS];
  int ndims_cr;
  ng_size_t dsizes_cr[NCL_MAX_DIMENSIONS];
  int ndims_ci;
  ng_size_t dsizes_ci[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_br, type_bi, type_cr, type_ci;
/*
 * Output array variables
 */
  void    *u,  *v;
  double *du, *dv;
  float  *ru, *rv;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_u, type_v;
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhsgc;
  double *work, *wvhsgc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
  bi = (void*)NclGetArgValue(
           1,
           6,
           &ndims_bi, 
           dsizes_bi,
           NULL,
           NULL,
           &type_bi,
           DONT_CARE);
  cr = (void*)NclGetArgValue(
           2,
           6,
           &ndims_cr,
           dsizes_cr,
           NULL,
           NULL,
           &type_cr,
           DONT_CARE);
  ci = (void*)NclGetArgValue(
           3,
           6,
           &ndims_ci, 
           dsizes_ci,
           NULL,
           NULL,
           &type_ci,
           DONT_CARE);
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
 * The grids coming in must be at least 2-dimensional and have the same # of
 *  dimensions. 
 */
  if( ndims_br != ndims_bi || ndims_br != ndims_ci || ndims_br != ndims_cr ||
      ndims_bi != ndims_ci || ndims_bi != ndims_cr ||
      ndims_cr != ndims_ci || ndims_br < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
 * The input/output arrays must have the same number of dimensions and
 * all but the last two dimension sizes must be the same.
 */
  if( ndims_u != ndims_br || ndims_v != ndims_br ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_br-2; i++ ) {
    if( dsizes_u[i] != dsizes_br[i] || dsizes_v[i] != dsizes_br[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: The input/output arrays must have the same leftmost dimension sizes");
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
  dbr = coerce_input_double(br,type_br,total_size_in,0,NULL,NULL);
  dbi = coerce_input_double(bi,type_bi,total_size_in,0,NULL,NULL);
  dcr = coerce_input_double(cr,type_cr,total_size_in,0,NULL,NULL);
  dci = coerce_input_double(ci,type_ci,total_size_in,0,NULL,NULL);
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

  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork,&ildwork,&jer);
  NGCALLF(dvhsgc,DVHSGC)(&inlat,&inlon,&ityp,&i_nt,&dv[0],&du[0],
			 &iidvw,&ijdvw,dbr,dbi,dcr,dci,&imdab,&indab,
			 wvhsgc,&ilvhsgc,work,&ilwork,&ker);

  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("vhsgc","vhsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&du[j],&dv[j],work);
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


NhlErrorTypes vhsgC_W( void )
{
/*
 * Input array variables
 */
  void *bc;
  double *dbc;
  int ndims_bc;
  ng_size_t dsizes_bc[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_bc, type_nlon;
/*
 * Output array variables
 */
  double *duv;
  float  *ruv;
  int ndims_uv;
  ng_size_t dsizes_uv[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, ntnlatnlon, ntnlatnlat;
  ng_size_t total_size_in, total_size_out;
  ng_size_t i, j, idvw, jdvw, mdab, ndab, l1, l2;
  int ityp;
  int ier=0, jer=0, ker=0, mer=0;
  ng_size_t *nlon_dims;
  void *nlon_in;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lvhsgc;
  double *work, *wvhsgc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilvhsgc;
  int iidvw;
  int ijdvw;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  bc = (void*)NclGetArgValue(
           0,
           2,
           &ndims_bc,
           dsizes_bc,
           NULL,
           NULL,
           &type_bc,
           DONT_CARE);
  nlon_in = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_nlon,
           DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  nlon_dims = get_dimensions(nlon_in,1,type_nlon,"vhsgC");
  if(nlon_dims == NULL) 
    return(NhlFATAL);
  nlon = *nlon_dims;
  NclFree(nlon_dims);

/*
 * The grid coming in must be at least 3-dimensional.
 */
  if( ndims_bc < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_bc[0] != 4) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: The first dimension of the input array must be 4");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  nlat = dsizes_bc[ndims_bc-1];
  nlatnlon = nlon * nlat; 

  nt = 1;
  for(i = 1; i < ndims_bc-2; i++) nt *= dsizes_bc[i];
  ntnlatnlon = nt * nlatnlon;
  ntnlatnlat = nt * nlat * nlat;

  total_size_out = 2 * ntnlatnlon;
  total_size_in  = 4 * ntnlatnlat;
/*
 * Coerce bc.
 */
  dbc = coerce_input_double(bc,type_bc,total_size_in,0,NULL,NULL);
  if(dbc == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  duv  = (double*)calloc(total_size_out,sizeof(double));
  if( duv == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: Unable to allocate memory for uv array");
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

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lvhsgc > INT_MAX) ||
     (idvw > INT_MAX) ||
     (jdvw > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilvhsgc = (int) lvhsgc;
  iidvw = (int) idvw;
  ijdvw = (int) jdvw;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wvhsgc = (double*)calloc(lvhsgc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wvhsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dvhsgci,DVHSGCI)(&inlat,&inlon,wvhsgc,&ilvhsgc,dwork,&ildwork,&jer);
  i = ntnlatnlat;
  j = ntnlatnlon;
  NGCALLF(dvhsgc,DVHSGC)(&inlat,&inlon,&ityp,&i_nt,&duv[j],&duv[0],
			 &iidvw,&ijdvw,&dbc[0],&dbc[i],&dbc[2*i],&dbc[3*i],
			 &imdab,&indab,wvhsgc,&ilvhsgc,work,&ilwork,&ker);
  NclFree(wvhsgc);
  NclFree(work);
  NclFree(dwork);
  if(type_bc != NCL_double) NclFree(dbc);

  NGCALLF(dchkerr,DCHKERR)("vhsgC","vhsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeov,DMATGEOV)(&inlat,&inlon,&duv[j],&duv[j+ntnlatnlon],work);
    j += nlatnlon;
  }
/*
 * Free workspace array.
 */
  NclFree(work);

/*
 * Return
 */
  ndims_uv = ndims_bc;
  dsizes_uv[0] = 2;
  for( i = 1; i < ndims_uv-2; i++ ) dsizes_uv[i] = dsizes_bc[i];
  dsizes_uv[ndims_uv-2] = nlat;
  dsizes_uv[ndims_uv-1] = nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_bc != NCL_double) {
    ruv = (float*)calloc(total_size_out,sizeof(float));
    if (ruv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"vhsgC: Unable to allocate space for output array" );
      return(NhlFATAL);
    }
/*
 * Copy double values to float values.
 */
    for( i = 0; i < total_size_out; i++ ) ruv[i] = (float)duv[i];
/*
 * Free double precision array.
 */
    NclFree(duv);

    return(NclReturnValue((void*)ruv,ndims_uv,dsizes_uv,NULL,NCL_float,0));
  }
  else {
    return(NclReturnValue((void*)duv,ndims_uv,dsizes_uv,NULL,NCL_double,0));
  }
}


NhlErrorTypes shaec_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  void *a, *b;
  double *da, *db;
  float *ra, *rb;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * various
 */
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t lwork, ldwork, lshaec;
  double *work, *wshaec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshaec;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
          DONT_CARE);
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
 * Output arrays must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: The output arrays must be float or double");
    return(NhlFATAL);
  }

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
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL);
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

  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
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
    NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shaec performs the spherical harmonic analysis on a (scalar) equal 
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym = 0;
  idg  = nlat;
  jdg  = nlon;
  l1   = mdab;
  if(nlat % 2) {
    l2 = (nlat+1)/2;
  }
  else {
    l2 = (nlat+2)/2;
  }
  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  wshaec = (double*)calloc(lshaec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if((nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilshaec = (int) lshaec;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,&jer);
  NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&i_nt,dg,&iidg,&ijdg,da,db,
			 &imdab,&indab,wshaec,&ilshaec,work,&ilwork,&ker);

  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shaec","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
  void    *g;
  double *dg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  void *a, *b;
  double *da, *db;
  float *ra, *rb;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * various
 */
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t lwork, ldwork, lshagc;
  double *work, *wshagc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshagc;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
          DONT_CARE);
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
 * Output arrays must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: The output arrays must be float or double");
    return(NhlFATAL);
  }

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
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL);
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
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) || (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: nlat and/or nlon is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);
/*
 * shagc performs the spherical harmonic analysis on a (scalar) gaussian
 * grid(s) and returns the coefficients in array(s) a,b
 * Here the scalar grid is "g" 
 */
  isym = 0;
  idg  = nlat;
  jdg  = nlon;
  l1   = mdab;
  if(nlat % 2) {
    l2 = (nlat+1)/2;
  }
  else {
    l2 = nlat/2;
  }

  lwork  = nlat*(nlon*nt+max(3*l2,nlon));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(0,1-l1)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (double*)calloc(lshagc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  
  if((nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilshagc = (int) lshagc;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&i_nt,dg,&iidg,&ijdg,da,db,
			 &imdab,&indab,wshagc,&ilshagc,work,&ilwork,&ker);

  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shagc","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *g;
  double *dg;
  float *rg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * various
 */
  ng_size_t i, j;
  ng_size_t idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon;
  ng_size_t total_size_in, total_size_out;
  ng_size_t lwork, ldwork, lshsec;
  double *work, *wshsec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshsec;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
  b = (void*)NclGetArgValue(
           1,
           3,
           &ndims_b,
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           DONT_CARE);
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
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
  if(type_g != NCL_float && type_g != NCL_double) {
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
  da = coerce_input_double(a,type_a,total_size_in,0,NULL,NULL);
  db = coerce_input_double(b,type_b,total_size_in,0,NULL,NULL);
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

  wshsec = (double*)calloc(lshsec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilshsec = (int) lshsec;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,&jer);
  NGCALLF(dshsec,DSHSEC)(&inlat,&inlon,&isym,&i_nt,&dg[0],&iidg,&ijdg,da,db,
			 &imdab,&indab,wshsec,&ilshsec,work,&ilwork,&ker);

  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);

  NGCALLF(dchkerr,DCHKERR)("shsec","shsec",&ier,&jer,&ker,&mer,5,5);
/*
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsec: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *g;
  double *dg;
  float *rg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * various
 */
  ng_size_t i, j;
  int isym;
  ng_size_t idg, jdg, mdab, ndab, l1, l2;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t lwork, ldwork, lshsgc;
  double *work, *wshsgc, *dwork;
  int inlon;
  int inlat;
  int ilshsgc;
  int ilwork;
  int ildwork;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int i_nt;
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
           DONT_CARE);
  b = (void*)NclGetArgValue(
           1,
           3,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           DONT_CARE);
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
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The input arrays must be at least 2-dimensional and have the same number of dimensions");
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
  if(type_g != NCL_float && type_g != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: The output array must be float or double");
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
  da = coerce_input_double(a,type_a,total_size_in,0,NULL,NULL);
  db = coerce_input_double(b,type_b,total_size_in,0,NULL,NULL);
  if(da == NULL || db == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure g is double
 */
  dg = coerce_output_double(g,type_g,total_size_out);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for double precision output array");
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

  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (lwork > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (nt > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshsgc = (int) lshsgc;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  i_nt = (int) nt;

  NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
  NGCALLF(dshsgc,DSHSGC)(&inlat,&inlon,&isym,&i_nt,&dg[0],&iidg,&ijdg,da,db,
			 &imdab,&indab,wshsgc,&ilshsgc,work,&ilwork,&ker);

  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shsgc","shsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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


NhlErrorTypes shsgc_R42_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *tmp_a = NULL;
  double *tmp_b = NULL;
  int ndims_a;
  ng_size_t dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b;
  ng_size_t dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
/*
 * Output array variables
 */
  void *g;
  double *tmp_g = NULL;
  ng_size_t size_g;
  ng_size_t *dsizes_g;
  NclBasicDataTypes type_g;
/*
 * various
 */
  double *work;
  ng_size_t index_ab, index_g;
  int ret;
  ng_size_t nlat=108;
  ng_size_t i, lwork;
  ng_size_t size_leftmost, size_rightmost_g, size_rightmost_ab;

  int ilwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (void*)NclGetArgValue(
           0,
           2,
           &ndims_a, 
           dsizes_a,
           NULL,
           NULL,
           &type_a,
           DONT_CARE);
  b = (void*)NclGetArgValue(
           1,
           2,
           &ndims_b, 
           dsizes_b,
           NULL,
           NULL,
           &type_b,
           DONT_CARE);

/*
 * The grids coming in must be at least 2-dimensional and have the same # of
 * dimensions, and the rightmost dimensions must be 43 x 43.
 */
  size_rightmost_ab = 43 * 43;
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  if(dsizes_a[ndims_a-1] != 43 || dsizes_a[ndims_a-2] != 43 || 
     dsizes_b[ndims_b-1] != 43 || dsizes_b[ndims_b-2] != 43) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: The rightmost two dimensions must be 43 x 43");
      return(NhlFATAL);
  }
/*
 * Calculate size of leftmost dimensions as we loop through dimentions
 * and make sure a and b are the same size.
 */
  dsizes_g = (ng_size_t*)calloc(ndims_a,sizeof(ng_size_t));
  size_leftmost = 1;
  for( i = 0; i < ndims_a-2; i++ ) {
    size_leftmost *= dsizes_a[i];
    dsizes_g[i] = dsizes_a[i];
    if( dsizes_a[i] != dsizes_b[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: a and b must have the same dimension sizes.");
      return(NhlFATAL);
    }
  }
  dsizes_g[ndims_a-2] = 108;
  dsizes_g[ndims_a-1] = 128;
  size_rightmost_g = 108 * 128;
  size_g = size_leftmost * size_rightmost_g;

/*
 * Create temporary double arrays for a and b, if either one of them
 * is not already double.
 */
  if(type_a != NCL_double) {
    tmp_a = (double*)calloc(size_rightmost_ab,sizeof(double));
    if(tmp_a == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
  }

  if(type_b != NCL_double) {
    tmp_b = (double*)calloc(size_rightmost_ab,sizeof(double));
    if(tmp_b == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Test dimension size
 */
  lwork = 4*nlat*(nlat+1)+2;
  if(lwork > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: lwork = %ld is greater than INT_MAX", lwork);
    return(NhlFATAL);
  }
  ilwork = (int) lwork;

/*
 * Allocate space for work array.
 */
  work  = (double*)calloc(lwork,sizeof(double));
  if(work == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
/*
 * Allocate space for temporary input array. The temporary array
 * tmp_g is just big enough to hold a 2-dimensional subsection of the
 * g array (which is always size 108 x 128).
 */
  if(type_a != NCL_double && type_b != NCL_double) {
    type_g = NCL_float;
    g      = (void*)calloc(size_g,sizeof(float));
    tmp_g  = (double*)calloc(size_rightmost_g,sizeof(double));
    if(g == NULL || tmp_g == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_g = NCL_double;
    g      = (void*)calloc(size_g,sizeof(double));
    if(g == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgc_R42: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  } 

/*
 * Loop through leftmost dimensions and call Fortran routine.
 */
  index_g  = 0;
  index_ab = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_a != NCL_double) {
/*
 * Coerce 43 x 43 subsection of a (tmp_a) to double.
 */
      coerce_subset_input_double(a,tmp_a,index_ab,type_a,size_rightmost_ab,
				 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_a to appropriate location in a.
 */
      tmp_a = &((double*)a)[index_ab];
    }
    if(type_b != NCL_double) {
/*
 * Coerce 43 x 43 subsection of b (tmp_b) to double.
 */
      coerce_subset_input_double(b,tmp_b,index_ab,type_b,size_rightmost_ab,
				 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_b to appropriate location in b.
 */
      tmp_b = &((double*)b)[index_ab];
    }

/*
 * Point tmp_g to appropriate location in g if necessary.
 */
    if(type_g == NCL_double) {
      tmp_g = &((double*)g)[index_g];
    }

    NGCALLF(dshsgcr42,DSHSGCR42)(tmp_a,tmp_b,tmp_g,work,&ilwork);

    if(type_g == NCL_float) {
      coerce_output_float_only(g,tmp_g,size_rightmost_g,index_g);
    }
    index_g  += size_rightmost_g;
    index_ab += size_rightmost_ab;
  }
/*
 * Free memory.
 */
  if(type_a != NCL_double) NclFree(tmp_a);
  if(type_b != NCL_double) NclFree(tmp_b);
  if(type_g != NCL_double) NclFree(tmp_g);

  ret = NclReturnValue(g,ndims_a,dsizes_g,NULL,type_g,0);
  NclFree(dsizes_g);
  NclFree(work);

  return(ret);
}


NhlErrorTypes shaeC_W( void )
{
/*
 * Input array variables
 */
  void *g;
  double *dg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  double *dab;
  float  *rab;
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lshaec;
  double *work, *wshaec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshaec;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);

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
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate space for output array.
 */
  dab  = (double*)calloc(total_size_out,sizeof(double));
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }

  if((nlon > INT_MAX) ||
     (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);

/*
 * Calculate work space sizes.
 */
  isym = 0;
  idg  = nlat;
  jdg  = nlon;
  l1   = mdab;
  if(nlat % 2) {
    l2 = (nlat+1)/2;
  }
  else {
    l2 = (nlat+2)/2;
  }

  lwork  = max(2*(nlat+1),nlat*(nt*nlon+max(3*l2,nlon)));
  lshaec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+nlon+15;
  ldwork = nlat+1;

  if((nt > INT_MAX) ||
     (lshaec > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilshaec = (int) lshaec;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  wshaec = (double*)calloc(lshaec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshaec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

  NGCALLF(dshaeci,DSHAECI)(&inlat,&inlon,wshaec,&ilshaec,dwork,&ildwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshaec,DSHAEC)(&inlat,&inlon,&isym,&i_nt,dg,&iidg,&ijdg,&dab[0],&dab[j],
			 &imdab,&indab,wshaec,&ilshaec,work,&ilwork,&ker);

  NclFree(wshaec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shaeC","shaec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shaeC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
    rab = (float*)calloc(total_size_out,sizeof(float));
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
  void    *g;
  double *dg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_g;
/*
 * Output array variables
 */
  double *dab;
  float  *rab;
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lshagc;
  double *work, *wshagc, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshagc;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
             DONT_CARE);

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
  dg = coerce_input_double(g,type_g,total_size_in,0,NULL,NULL);
  if(dg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array.
 */
  dab  = (double*)calloc(total_size_out,sizeof(double));
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for coefficient arrays");
    return(NhlFATAL);
  }

/*
 * Determine the workspace size.
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;

/*
 * transform from geophysical coordinates to math coordinates.
 * (geo) nlon is the last dim.
 */
  j = 0;
  for(i = 0; i < nt; i++ ) {
    NGCALLF(dgeomat,DGEOMAT)(&inlon,&inlat,&dg[j],work);
    j += nlatnlon;
  }
  NclFree(work);

/*
 * Calculate work space sizes.
 */
  isym = 0;
  idg  = nlat;
  jdg  = nlon;
  l1   = mdab;
  if(nlat % 2) {
    l2 = (nlat+1)/2;
  }
  else {
    l2 = nlat/2;
  }

  lwork  = nlat*(nlon*nt+max(3*l2,nlon));
  lshagc = nlat*(2*l2+3*l1-2)+3*l1*max(0,1-l1)/2+nlon+15;
  ldwork = nlat*(nlat+4);

  wshagc = (double*)calloc(lshagc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshagc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  if((nt > INT_MAX) ||
     (lshagc > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  i_nt = (int) nt;
  ilshagc = (int) lshagc;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  j = nt * ndab * mdab;
  NGCALLF(dshagci,DSHAGCI)(&inlat,&inlon,wshagc,&ilshagc,dwork,&ildwork,&jer);
  NGCALLF(dshagc,DSHAGC)(&inlat,&inlon,&isym,&i_nt,dg,&iidg,&ijdg,&dab[0],&dab[j],
			 &imdab,&indab,wshagc,&ilshagc,work,&ilwork,&ker);

  NclFree(wshagc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shagC","shagc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shagC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
    rab = (float*)calloc(total_size_out,sizeof(float));
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
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_nlon;
/*
 * Output array variables
 */
  double *dg;
  float *rg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
  ng_size_t *nlon_dims;
  void *nlon_in;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lshsec;
  double *work, *wshsec, *dwork;
  int inlon;
  int inlat;
  int i_nt;
  int ilshsec;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int ildwork;
  int ilwork;
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
           DONT_CARE);
/*
 * Get nlon
 */
  nlon_in = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_nlon,
           DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  nlon_dims = get_dimensions(nlon_in,1,type_nlon,"shseC");
  if(nlon_dims == NULL) 
    return(NhlFATAL);
  nlon = *nlon_dims;
  NclFree(nlon_dims);

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
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nlatnlon = nlon * nlat;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_in  = 2 * nt * mdab * ndab;
  total_size_out = nt * nlon * nlat;
/*
 * Coerce ab.
 */
  dab = coerce_input_double(ab,type_ab,total_size_in,0,NULL,NULL);
  if(dab == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }

/*
 * Compute size for output array.
 */
  dg = (double *)calloc(total_size_out,sizeof(double));
  if( dg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for output array");
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
  lshsec = 2*nlat*l2+3*(max(l1-2,0)*(2*nlat-l1-1))/2+ nlon +15;
  ldwork = nlat+1;

  wshsec = (double*)calloc(lshsec,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshsec == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (nt > INT_MAX) ||
     (lshsec > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  i_nt = (int) nt;
  ilshsec = (int) lshsec;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;

  j = nt * ndab * mdab;
  NGCALLF(dshseci,DSHSECI)(&inlat,&inlon,wshsec,&ilshsec,dwork,&ildwork,&jer);
  NGCALLF(dshsec,DSHSEC)(&inlat,&inlon,&isym,&i_nt,&dg[0],&iidg,&ijdg,
			 &dab[0],&dab[j],
			 &imdab,&indab,wshsec,&ilshsec,work,&ilwork,&ker);

  NclFree(wshsec);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shseC","shsec",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shseC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
  dsizes_g[ndims_g-1] = nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_ab != NCL_double) {
    rg = (float*)calloc(total_size_out,sizeof(float));
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
  int ndims_ab;
  ng_size_t dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab, type_nlon;
/*
 * Output array variables
 */
  double *dg;
  float *rg;
  int ndims_g;
  ng_size_t dsizes_g[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  ng_size_t nt, nlat, nlon, nlatnlon, total_size_in, total_size_out;
  ng_size_t i, j, idg, jdg, mdab, ndab, l1, l2;
  int isym;
  int ier=0, jer=0, ker=0, mer=0;
  ng_size_t *nlon_dims;
  void *nlon_in;
/*
 * Workspace variables
 */
  ng_size_t lwork, ldwork, lshsgc;
  double *work, *wshsgc, *dwork;
  int inlon;
  int inlat;
  int ilshsgc;
  int ilwork;
  int ildwork;
  int iidg;
  int ijdg;
  int imdab;
  int indab;
  int i_nt;
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
           DONT_CARE);
/*
 * Get nlon
 */
  nlon_in = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_nlon,
           DONT_CARE);

/*
 * Convert the input dimensions to ng_size_t.
 */
  nlon_dims = get_dimensions(nlon_in,1,type_nlon,"shsgC");
  if(nlon_dims == NULL) 
    return(NhlFATAL);
  nlon = *nlon_dims;
  NclFree(nlon_dims);

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
  if (nlon % 2) {
    mdab   = min(nlat,(nlon+1)/2);
  }
  else {
    mdab   = min(nlat,(nlon+2)/2);
  }
  if(dsizes_ab[ndims_ab-1] != mdab) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: The last dimension of the coefficient array must be min(nlat,(nlon+2)/2) if nlon is even or min(nlat,(nlon+1)/2) if nlon is odd");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nlatnlon = nlon * nlat;

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_in  = 2 * nt * mdab * ndab;
  total_size_out = nt * nlon * nlat;
/*
 * Coerce ab.
 */
  dab = coerce_input_double(ab,type_ab,total_size_in,0,NULL,NULL);
  if(dab == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for coercing input array to double precision");
    return(NhlFATAL);
  }
/*
 * Compute size for output array.
 */
  dg = (double *)calloc(total_size_out,sizeof(double));
  if( dg == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Calculate work space sizes.
 */
  isym   = 0;
  idg    = nlat;
  jdg    = nlon;
  l1     = mdab;
  if (nlat % 2) {
    l2 = (nlat+1)/2;
  }
  else {
    l2 = nlat/2;
  }

  lwork  = nlat*(nlon*nt+max(3*l2,nlon));
  ldwork = nlat*(nlat+4);
  lshsgc = nlat*(2*l2+3*l1-2)+3*l1*max(1-l1,0)/2+ nlon +15;

  wshsgc = (double*)calloc(lshsgc,sizeof(double));
  work   = (double*)calloc( lwork,sizeof(double));
  dwork  = (double*)calloc(ldwork,sizeof(double));

  if( wshsgc == NULL || work == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  if((nlon > INT_MAX) ||
     (nlat > INT_MAX) ||
     (lshsgc > INT_MAX) ||
     (lwork > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (idg > INT_MAX) ||
     (jdg > INT_MAX) ||
     (mdab > INT_MAX) ||
     (ndab > INT_MAX) ||
     (nt > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlon = (int) nlon;
  inlat = (int) nlat;
  ilshsgc = (int) lshsgc;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  iidg = (int) idg;
  ijdg = (int) jdg;
  imdab = (int) mdab;
  indab = (int) ndab;
  i_nt = (int) nt;

  NGCALLF(dshsgci,DSHSGCI)(&inlat,&inlon,wshsgc,&ilshsgc,dwork,&ildwork,&jer);
  j = nt * ndab * mdab;
  NGCALLF(dshsgc,DSHSGC)(&inlat,&inlon,&isym,&i_nt,&dg[0],&iidg,&ijdg,
			 &dab[0],&dab[j],
			 &imdab,&indab,wshsgc,&ilshsgc,work,&ilwork,&ker);
  NclFree(wshsgc);
  NclFree(work);
  NclFree(dwork);
  NGCALLF(dchkerr,DCHKERR)("shsgC","shsgc",&ier,&jer,&ker,&mer,5,5);
/* 
 * transform from math coordinates to geophysical coordinates
 * (math) nlon is the first dim
 */
  lwork = nlatnlon;
  work  = (double*)calloc(lwork,sizeof(double));
  if( work == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"shsgC: Unable to allocate memory for work array");
    return(NhlFATAL);
  }
  j = 0;
  for( i = 0; i < nt; i++ ) {
    NGCALLF(dmatgeo,DMATGEO)(&inlat,&inlon,&dg[j],work);
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
  dsizes_g[ndims_g-1] = nlon;
/*
 * Determine whether to return float or double.
 */
  if(type_ab != NCL_double) {
    rg = (float*)calloc(total_size_out,sizeof(float));
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

NhlErrorTypes set_sphere_radius_W(void)
{
/*
 * Input array variables
 */
  void *radius;
  NclBasicDataTypes type_radius;

/*
 * Retrieve argument #1
 */
  radius = (void *) NclGetArgValue(
          0,
          1,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_radius,
          DONT_CARE);

  _Nclcoerce((NclTypeClass)nclTypedoubleClass,&scale,radius,1,NULL,NULL,
     _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_radius)));
  return(NhlNOERROR);
}


NhlErrorTypes get_sphere_radius_W(void)
{
  ng_size_t dsizes[1];

  dsizes[0] = 1;
  return(NclReturnValue((void*)&scale,1,dsizes,NULL,NCL_double,0));
}


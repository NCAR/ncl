#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(trssphx,TRSSPHX)(int *,int *,int *,int *,double *,int *,
                                     int *,int *,double *, double *,int *,
                                     int *,double *,int *,int *, double *,
                                     int *,int *,int *);

extern void NGCALLF(dfo2f,DFO2F)(double *,int *,int *,double *,int *,double *,
                                 int *,double *,int *,int *,int *);

extern void compute_jlatilon(ng_size_t *,int,ng_size_t *,ng_size_t *,ng_size_t *,
                             ng_size_t *,ng_size_t *,ng_size_t *,
                             ng_size_t *,ng_size_t *,int);

NhlErrorTypes g2gsh_W( void )
{
/*
 * Input array variables
 */
  void *Ta, *tmp_dsizes_Tb;
  double *tmp_Ta = NULL;
  int ndims_Ta;
  ng_size_t dsizes_Ta[NCL_MAX_DIMENSIONS], *dsizes_Tb;
  NclScalar missing_Ta, missing_dTa, missing_rTa;
  NclBasicDataTypes type_Ta, type_Tb, type_dsizes_Tb;
  int has_missing_Ta, found_missing;
  ng_size_t nlata, nlona;
  int igrida[2];
/*
 * Output array variables
 */
  void *Tb;
  double *tmp_Tb = NULL;
  ng_size_t dsizes_Tb2[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2], ret;
/*
 * various
 */
  int *twave, intl;
  ng_size_t i, index_Ta, index_Tb;
  ng_size_t total_size_leftmost, nlatanlona, nlatbnlonb;
  ng_size_t total_size_Ta, total_size_Tb;
  int inlona, inlata, inlonb, inlatb, ilwork, ildwork, ilsave; 
  int ilwkmin, ilsvmin;
/*
 * Workspace variables
 */
  ng_size_t lsave, lsvmin, lwork, ldwork, lwkmin;
  ng_size_t klat, klon, k1, k2, lwa, lwb;
  int ier = 0, kmiss = 0;
  double *work, *wsave, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (void*)NclGetArgValue(
           0,
           3,
           &ndims_Ta, 
           dsizes_Ta,
           &missing_Ta,
           &has_missing_Ta,
           &type_Ta,
           DONT_CARE);
/*
 * Get sizes for output array.
 */
  tmp_dsizes_Tb = (void*)NclGetArgValue(
                1,
                3,
                NULL,
                NULL,
                NULL,
                NULL,
                &type_dsizes_Tb,
                DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  dsizes_Tb = get_dimensions(tmp_dsizes_Tb,2,type_dsizes_Tb,"g2gsh");
  if(dsizes_Tb == NULL) 
    return(NhlFATAL);

/*
 * Get optional wave truncation value.
 */
  twave = (int*)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatanlona(dsizes_Ta,dsizes_Tb,ndims_Ta,2,
                     &nlata,&nlona,&nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_size_leftmost,&total_size_Ta,&total_size_Tb);
  NclFree(dsizes_Tb);

/*
 * Check output dimensions.
 */
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ta,has_missing_Ta,&missing_Ta,&missing_dTa,
                 &missing_rTa);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ta != NCL_double) {
    type_Tb = NCL_float;
    Tb = (void*)calloc(total_size_Tb,sizeof(float));
    tmp_Tb = (double*)calloc(nlatbnlonb,sizeof(double));
    tmp_Ta = (double*)calloc(nlatanlona,sizeof(double));
    if(Tb == NULL || tmp_Tb == NULL || tmp_Ta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_Tb = NCL_double;
    Tb = (void*)calloc(total_size_Tb,sizeof(double));
    if(Tb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = 2;
  igrida[1] = igridb[1] = 0;

  intl = 0;

/*
 * Determine the workspace size.
 */
  klat  = nlata > nlatb ? nlata : nlatb;
  klon  = nlona > nlonb ? nlona : nlonb;
  k1    = klat < ((klon+2)/2) ? klat : ((klon+2)/2);
  k2    = (klat+1)/2;
  lwa   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lwb   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lsave = lwb + lwa + 2*(klon+15);
  lwork = klat*(4*k1+ klon+ 2*klat+ 4) + 3*((k1-2)*2*(2*klat-k1-1))/2;
  ldwork = klat*(klat+4);

  lsave = 5*lsave;     /* the above are only approximate */
  lwork = 5*lwork;

/*
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) || (nlata > INT_MAX) || (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) || (lwork > INT_MAX) || (ldwork > INT_MAX) ||
     (lsave > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }

/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_Ta = index_Tb = 0;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_Ta != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ta (tmp_Ta) to double.
 */
      coerce_subset_input_double(Ta,tmp_Ta,index_Ta,type_Ta,nlatanlona,0,
                                 &missing_Ta,&missing_dTa);
    }
    else {
/*
 * Point tmp_Ta and tmp_Tb to appropriate locations in Ta and Tb.
 */
      tmp_Ta = &((double*)Ta)[index_Ta];
      tmp_Tb = &((double*)Tb)[index_Tb];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_Ta,nlatanlona,has_missing_Ta,
                                     missing_dTa.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(Tb,index_Tb,type_Ta,nlatbnlonb,
                                missing_dTa.doubleval);
    }
    else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
      NGCALLF(trssphx,TRSSPHX)(&intl,igrida,&inlona,&inlata,tmp_Ta,igridb,
			       &inlonb,&inlatb,tmp_Tb,wsave,&ilsave,&ilsvmin,
			       work,&ilwork,&ilwkmin,dwork,&ildwork,&ier,twave);
      lwkmin = (ng_size_t) ilwkmin;
      lsvmin = (ng_size_t) ilsvmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gsh: ier = %d\n", ier );
      }
/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
      if(type_Tb == NCL_float) {
        coerce_output_float_only(Tb,tmp_Tb,nlatbnlonb,index_Tb);
      } 
    }
    index_Ta += nlatanlona;
    index_Tb += nlatbnlonb;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gsh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) NclFree(work);
  if(dwork != NULL) NclFree(dwork);
  if(wsave != NULL) NclFree(wsave);
  if(type_Ta != NCL_double) {
    NclFree(tmp_Ta);
    NclFree(tmp_Tb);
  }
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_Ta) {
    if(type_Tb == NCL_float) {
      ret = NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_rTa,type_Tb,0);
    }
    else {
      ret = NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_dTa,type_Tb,0);
    }
  }
  else {
    ret = NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,NULL,type_Tb,0);
  }
  return(ret);
}


NhlErrorTypes f2gsh_W( void )
{
/*
 * Input array variables
 */
  void *Ta, *tmp_dsizes_Tb;
  ng_size_t *dsizes_Tb;
  double *tmp_Ta = NULL;
  int ndims_Ta;
  ng_size_t dsizes_Ta[NCL_MAX_DIMENSIONS];
  NclScalar missing_Ta, missing_dTa, missing_rTa;
  NclBasicDataTypes type_Ta, type_Tb, type_dsizes_Tb;
  int has_missing_Ta, found_missing;
  int igrida[2];
/*
 * Output array variables
 */
  void *Tb;
  double *tmp_Tb = NULL;
  ng_size_t dsizes_Tb2[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  ng_size_t nlatb, nlonb;
  int igridb[2];
/*
 * various
 */
  ng_size_t i, index_Ta, index_Tb;
  int *twave, intl;
  ng_size_t total_size_leftmost, nlatanlona, nlatbnlonb;
  ng_size_t total_size_Ta, total_size_Tb;
  int inlona, inlata, inlonb, inlatb, ilwork, ildwork, ilsave;
  int ilwkmin, ilsvmin;
/*
 * Workspace variables
 */
  ng_size_t lsave, lsvmin, lwork, ldwork, lwkmin;
  ng_size_t klat, klon, k1, k2, lwa, lwb;
  int ier = 0, kmiss = 0;
  double *work, *wsave, *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (void*)NclGetArgValue(
           0,
           3,
           &ndims_Ta, 
           dsizes_Ta,
           &missing_Ta,
           &has_missing_Ta,
           &type_Ta,
           DONT_CARE);
/*
 * Get sizes for output array.
 */
  tmp_dsizes_Tb = (void*)NclGetArgValue(
                1,
                3,
                NULL,
                NULL,
                NULL,
                NULL,
                &type_dsizes_Tb,
                DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  dsizes_Tb = get_dimensions(tmp_dsizes_Tb,2,type_dsizes_Tb,"f2gsh");
  if(dsizes_Tb == NULL) 
    return(NhlFATAL);
/*
 * Get optional wave truncation value.
 */
  twave = (int*)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatanlona(dsizes_Ta,dsizes_Tb,ndims_Ta,2,
                     &nlata,&nlona,&nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_size_leftmost,&total_size_Ta,&total_size_Tb);
  NclFree(dsizes_Tb);
/*
 * Check output dimensions.
 */
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ta,has_missing_Ta,&missing_Ta,&missing_dTa,
                 &missing_rTa);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ta != NCL_double) {
    type_Tb = NCL_float;
    Tb = (void*)calloc(total_size_Tb,sizeof(float));
    tmp_Tb = (double*)calloc(nlatbnlonb,sizeof(double));
    tmp_Ta = (double*)calloc(nlatanlona,sizeof(double));
    if(Tb == NULL || tmp_Tb == NULL || tmp_Ta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_Tb = NCL_double;
    Tb = (void*)calloc(total_size_Tb,sizeof(double));
    if(Tb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = 1;
  igridb[0] = 2;
  igrida[1] = igridb[1] =  0;

  intl = 0;

/*
 * Determine the workspace size.
 */
  klat  = nlata > nlatb ? nlata : nlatb;
  klon  = nlona > nlonb ? nlona : nlonb;
  k1    = klat < ((klon+2)/2) ? klat : ((klon+2)/2);
  k2    = (klat+1)/2;
  lwa   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lwb   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lsave = lwb + lwa + 2*(klon+15);
  lwork = klat*(4*k1+ klon+ 2*klat+ 4) + 3*((k1-2)*2*(2*klat-k1-1))/2;
  ldwork = klat*(klat+4);

  lsave = 5*lsave;     /* the above are only approximate */
  lwork = 5*lwork;

/*
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) || (nlata > INT_MAX) || (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) || (lwork > INT_MAX) || (ldwork > INT_MAX) ||
     (lsave > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }

/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_Ta = index_Tb = 0;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_Ta != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ta (tmp_Ta) to double.
 */
      coerce_subset_input_double(Ta,tmp_Ta,index_Ta,type_Ta,nlatanlona,0,
                                 &missing_Ta,&missing_dTa);
    }
    else {
/*
 * Point tmp_Ta and tmp_Tb to appropriate locations in Ta and Tb.
 */
      tmp_Ta = &((double*)Ta)[index_Ta];
      tmp_Tb = &((double*)Tb)[index_Tb];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_Ta,nlatanlona,has_missing_Ta,
                                     missing_dTa.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(Tb,index_Tb,type_Ta,nlatbnlonb,
                                missing_dTa.doubleval);
    }
    else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
      NGCALLF(trssphx,TRSSPHX)(&intl,igrida,&inlona,&inlata,tmp_Ta,igridb,
			       &inlonb,&inlatb,tmp_Tb,wsave,&ilsave,&ilsvmin,
			       work,&ilwork,&ilwkmin,dwork,&ildwork,&ier,twave);
      lwkmin = (ng_size_t) ilwkmin;
      lsvmin = (ng_size_t) ilsvmin;
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gsh: ier = %d\n", ier );
      }

/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
      if(type_Tb == NCL_float) {
        coerce_output_float_only(Tb,tmp_Tb,nlatbnlonb,index_Tb);
      } 
    }
    index_Ta += nlatanlona;
    index_Tb += nlatbnlonb;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gsh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) NclFree(work);
  if(dwork != NULL) NclFree(dwork);
  if(wsave != NULL) NclFree(wsave);
  if(type_Ta != NCL_double) {
    NclFree(tmp_Ta);
    NclFree(tmp_Tb);
  }
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_Ta) {
    if(type_Tb == NCL_float) {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_rTa,type_Tb,0));
    }
    else {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_dTa,type_Tb,0));
    }
  }
  else {
    return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,NULL,type_Tb,0));
  }
}


NhlErrorTypes g2fsh_W( void )
{
/*
 * Input array variables
 */
  void *Ta, *tmp_dsizes_Tb;
  ng_size_t *dsizes_Tb;
  double *tmp_Ta = NULL;
  int ndims_Ta;
  ng_size_t dsizes_Ta[NCL_MAX_DIMENSIONS];
  NclScalar missing_Ta, missing_dTa, missing_rTa;
  NclBasicDataTypes type_Ta, type_Tb, type_dsizes_Tb;
  int has_missing_Ta, found_missing;
  int igrida[2];
  int twave = 0;
/*
 * Output array variables
 */
  void *Tb;
  double *tmp_Tb = NULL;
  ng_size_t dsizes_Tb2[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  ng_size_t nlatb, nlonb;
  int igridb[2];
/*
 * various
 */
  int intl;
  ng_size_t i, index_Ta, index_Tb;
  ng_size_t total_size_leftmost, nlatanlona, nlatbnlonb;
  ng_size_t total_size_Ta, total_size_Tb;
/*
 * Workspace variables
 */
  ng_size_t lsave, lsvmin, lwork, ldwork, lwkmin;
  ng_size_t klat, klon, k1, k2, lwa, lwb;
  int ier = 0, kmiss = 0;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ilwork, ildwork, ilsave;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (void*)NclGetArgValue(
           0,
           2,
           &ndims_Ta, 
           dsizes_Ta,
           &missing_Ta,
           &has_missing_Ta,
           &type_Ta,
           DONT_CARE);
/*
 * Get sizes for output array.
 */
  tmp_dsizes_Tb = (void*)NclGetArgValue(
                1,
                2,
                NULL,
                NULL,
                NULL,
                NULL,
                &type_dsizes_Tb,
                DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  dsizes_Tb = get_dimensions(tmp_dsizes_Tb,2,type_dsizes_Tb,"g2fsh");
  if(dsizes_Tb == NULL) 
    return(NhlFATAL);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatanlona(dsizes_Ta,dsizes_Tb,ndims_Ta,2,
                     &nlata,&nlona,&nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_size_leftmost,&total_size_Ta,&total_size_Tb);
  NclFree(dsizes_Tb);
/*
 * Check output dimensions.
 */
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ta,has_missing_Ta,&missing_Ta,&missing_dTa,
                 &missing_rTa);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ta != NCL_double) {
    type_Tb = NCL_float;
    Tb = (void*)calloc(total_size_Tb,sizeof(float));
    tmp_Tb = (double*)calloc(nlatbnlonb,sizeof(double));
    tmp_Ta = (double*)calloc(nlatanlona,sizeof(double));
    if(Tb == NULL || tmp_Tb == NULL || tmp_Ta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_Tb = NCL_double;
    Tb = (void*)calloc(total_size_Tb,sizeof(double));
    if(Tb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = 2;
  igridb[0] = 1;
  igrida[1] = igridb[1] =  0;

  intl = 0;

/*
 * Determine the workspace size.
 */
  klat  = nlata > nlatb ? nlata : nlatb;
  klon  = nlona > nlonb ? nlona : nlonb;
  k1    = klat < ((klon+2)/2) ? klat : ((klon+2)/2);
  k2    = (klat+1)/2;
  lwa   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lwb   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lsave = lwb + lwa + 2*(klon+15);
  lwork = klat*(4*k1+ klon+ 2*klat+ 4) + 3*((k1-2)*2*(2*klat-k1-1))/2;
  ldwork = klat*(klat+4);

  lsave = 5*lsave;     /* the above are only approximate */
  lwork = 5*lwork;

/*
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) || (nlata > INT_MAX) || (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) || (lwork > INT_MAX) || (ldwork > INT_MAX) ||
     (lsave > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_Ta = index_Tb = 0;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_Ta != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ta (tmp_Ta) to double.
 */
      coerce_subset_input_double(Ta,tmp_Ta,index_Ta,type_Ta,nlatanlona,0,
                                 &missing_Ta,&missing_dTa);
    }
    else {
/*
 * Point tmp_Ta and tmp_Tb to appropriate locations in Ta and Tb.
 */
      tmp_Ta = &((double*)Ta)[index_Ta]; 
      tmp_Tb = &((double*)Tb)[index_Tb];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_Ta,nlatanlona,has_missing_Ta,
                                     missing_dTa.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(Tb,index_Tb,type_Ta,nlatbnlonb,
                                missing_dTa.doubleval);
    }
    else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
      NGCALLF(trssphx,TRSSPHX)(&intl,igrida,&inlona,&inlata,tmp_Ta,igridb,
			       &inlonb,&inlatb,tmp_Tb,wsave,&ilsave,&ilsvmin,
			       work,&ilwork,&ilwkmin,dwork,&ildwork,&ier,&twave);
      lwkmin = (ng_size_t) ilwkmin;
      lsvmin = (ng_size_t) ilsvmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fsh: ier = %d\n", ier );
      }
/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
      if(type_Tb == NCL_float) {
        coerce_output_float_only(Tb,tmp_Tb,nlatbnlonb,index_Tb);
      } 
    }
    index_Ta += nlatanlona;
    index_Tb += nlatbnlonb;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fsh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) NclFree(work);
  if(dwork != NULL) NclFree(dwork);
  if(wsave != NULL) NclFree(wsave);
  if(type_Ta != NCL_double) {
    NclFree(tmp_Ta);
    NclFree(tmp_Tb);
  }
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_Ta) {
    if(type_Tb == NCL_float) {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_rTa,type_Tb,0));
    }
    else {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_dTa,type_Tb,0));
    }
  }
  else {
    return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,NULL,type_Tb,0));
  }
}



NhlErrorTypes f2fsh_W( void )
{
/*
 * Input array variables
 */
  void *Ta, *tmp_dsizes_Tb;
  ng_size_t *dsizes_Tb;
  double *tmp_Ta = NULL;
  int ndims_Ta;
  ng_size_t dsizes_Ta[NCL_MAX_DIMENSIONS];
  NclScalar missing_Ta, missing_dTa, missing_rTa;
  NclBasicDataTypes type_Ta, type_Tb, type_dsizes_Tb;
  int has_missing_Ta, found_missing;
  ng_size_t nlata, nlona;
  int igrida[2];
  int twave = 0;
/*
 * Output array variables
 */
  void *Tb;
  double *tmp_Tb = NULL;
  ng_size_t dsizes_Tb2[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2];
/*
 * various
 */
  int intl;
  ng_size_t i, index_Ta, index_Tb;
  ng_size_t total_size_leftmost, nlatanlona, nlatbnlonb;
  ng_size_t total_size_Ta, total_size_Tb;
/*
 * Workspace variables
 */
  ng_size_t lsave, lsvmin, lwork, ldwork, lwkmin;
  ng_size_t klat, klon, k1, k2, lwa, lwb;
  int ier = 0, kmiss = 0;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ilwork, ildwork, ilsave;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (void*)NclGetArgValue(
           0,
           2,
           &ndims_Ta, 
           dsizes_Ta,
           &missing_Ta,
           &has_missing_Ta,
           &type_Ta,
           DONT_CARE);
/*
 * Get sizes for output array.
 */
  tmp_dsizes_Tb = (void*)NclGetArgValue(
                1,
                2,
                NULL,
                NULL,
                NULL,
                NULL,
                &type_dsizes_Tb,
                DONT_CARE);
/*
 * Convert the input dimensions to ng_size_t.
 */
  dsizes_Tb = get_dimensions(tmp_dsizes_Tb,2,type_dsizes_Tb,"f2fsh");
  if(dsizes_Tb == NULL) 
    return(NhlFATAL);

/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Compute the total number of elements in our array.
 */
  compute_nlatanlona(dsizes_Ta,dsizes_Tb,ndims_Ta,2,
                     &nlata,&nlona,&nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_size_leftmost,&total_size_Ta,&total_size_Tb);
  NclFree(dsizes_Tb);
/*
 * Check output dimensions.
 */
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ta,has_missing_Ta,&missing_Ta,&missing_dTa,
                 &missing_rTa);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ta != NCL_double) {
    type_Tb = NCL_float;
    Tb = (void*)calloc(total_size_Tb,sizeof(float));
    tmp_Tb = (double*)calloc(nlatbnlonb,sizeof(double));
    tmp_Ta = (double*)calloc(nlatanlona,sizeof(double));
    if(Tb == NULL || tmp_Tb == NULL || tmp_Ta == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_Tb = NCL_double;
    Tb = (void*)calloc(total_size_Tb,sizeof(double));
    if(Tb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = 1;
  igrida[1] = igridb[1] = 0;

  intl = 0;

/*
 * Determine the workspace size.
 */
  klat  = nlata > nlatb ? nlata : nlatb;
  klon  = nlona > nlonb ? nlona : nlonb;
  k1    = klat < ((klon+2)/2) ? klat : ((klon+2)/2);
  k2    = (klat+1)/2;
  lwa   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lwb   = 2*klat*k2 + 3*((k1-2)*(klat+klat-k1-1))/2;
  lsave = lwb + lwa + 2*(klon+15);
  lwork = klat*(4*k1+ klon+ 2*klat+ 4) + 3*((k1-2)*2*(2*klat-k1-1))/2;
  ldwork = klat*(klat+4);

  lsave = 5*lsave;     /* the above are only approximate */
  lwork = 5*lwork;

/*
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) || (nlata > INT_MAX) || (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) || (lwork > INT_MAX) || (ldwork > INT_MAX) ||
     (lsave > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ilwork = (int) lwork;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_Ta = index_Tb = 0;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_Ta != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ta (tmp_Ta) to double.
 */
      coerce_subset_input_double(Ta,tmp_Ta,index_Ta,type_Ta,nlatanlona,0,
                                 &missing_Ta,&missing_dTa);
    }
    else {
/*
 * Point tmp_Ta and tmp_Tb to appropriate locations in Ta and Tb.
 */
      tmp_Ta = &((double*)Ta)[index_Ta];
      tmp_Tb = &((double*)Tb)[index_Tb];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_Ta,nlatanlona,has_missing_Ta,
                                     missing_dTa.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(Tb,index_Tb,type_Ta,nlatbnlonb,
                                missing_dTa.doubleval);
    }
    else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
      NGCALLF(trssphx,TRSSPHX)(&intl,igrida,&inlona,&inlata,tmp_Ta,igridb,
			       &inlonb,&inlatb,tmp_Tb,wsave,&ilsave,&ilsvmin,
			       work,&ilwork,&ilwkmin,dwork,&ildwork,&ier,&twave);
      lwkmin = (ng_size_t) ilwkmin;
      lsvmin = (ng_size_t) ilsvmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fsh: ier = %d\n", ier );
      }
/*
 * Copy output values from temporary array "tmp_Tb" to final array "Tb".
 */
      if(type_Tb == NCL_float) {
        coerce_output_float_only(Tb,tmp_Tb,nlatbnlonb,index_Tb);
      } 
    }
    index_Ta += nlatanlona;
    index_Tb += nlatbnlonb;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fsh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) NclFree(work);
  if(dwork != NULL) NclFree(dwork);
  if(wsave != NULL) NclFree(wsave);
  if(type_Ta != NCL_double) {
    NclFree(tmp_Ta);
    NclFree(tmp_Tb);
  }
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_Ta) {
    if(type_Tb == NCL_float) {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_rTa,type_Tb,0));
    }
    else {
      return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,&missing_dTa,type_Tb,0));
    }
  }
  else {
    return(NclReturnValue(Tb,ndims_Ta,dsizes_Tb2,NULL,type_Tb,0));
  }
}


NhlErrorTypes fo2fsh_W( void )
{
/*
 * Input array variables
 */
  void *goff;
  double *tmp_goff = NULL;
  int ndims_goff;
  ng_size_t jlat, jlat1, ilon;
  ng_size_t dsizes_goff[NCL_MAX_DIMENSIONS];
  NclScalar missing_goff, missing_dgoff, missing_rgoff;
  NclBasicDataTypes type_goff, type_greg;
  int has_missing_goff, found_missing;
/*
 * Output array variables
 */
  void *greg;
  double *tmp_greg = NULL;
  ng_size_t *dsizes_greg;
/*
 * Workspace variables
 */
  ng_size_t lwork, lsave;
  double *work, *wsave;
/*
 * error code, various
 */
  int ioff, ier = 0, kmiss = 0;
  ng_size_t i, index_goff, index_greg;
  ng_size_t total_size_leftmost, jlatilon, jlat1ilon;
  ng_size_t total_size_goff, total_size_greg, ret;
  int iilon, ijlat, ijlat1, ilwork, ilsave; 
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  goff = (void*)NclGetArgValue(
           0,
           1,
           &ndims_goff, 
           dsizes_goff,
           &missing_goff,
           &has_missing_goff,
           &type_goff,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_goff < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Get sizes for output array.
 */
   compute_jlatilon(dsizes_goff,ndims_goff,&jlat,&ilon,&jlatilon,
                   &jlat1,&jlat1ilon,&total_size_leftmost,&total_size_goff,
                   &total_size_greg,1);
/*
 * Coerce missing values.
 */
  coerce_missing(type_goff,has_missing_goff,&missing_goff,&missing_dgoff,
                 &missing_rgoff);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in goff and greg.
 */
  if(type_goff != NCL_double) {
    type_greg = NCL_float;
    greg = (void*)calloc(total_size_greg,sizeof(float));
    tmp_greg = (double*)calloc(jlat1ilon,sizeof(double));
    tmp_goff = (double*)calloc(jlatilon,sizeof(double));
    if(greg == NULL || tmp_greg == NULL || tmp_goff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_greg = NCL_double;
    greg = (void*)calloc(total_size_greg,sizeof(double));
    if(greg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for dimension sizes and output array.
 */
  dsizes_greg = (ng_size_t *)calloc(ndims_goff,sizeof(ng_size_t));
  if( dsizes_greg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: Unable to allocate memory for output dimension array");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_goff-2; i++ ) dsizes_greg[i] = dsizes_goff[i];
  dsizes_greg[ndims_goff-2] = jlat1;
  dsizes_greg[ndims_goff-1] = ilon;
/*
 * Determine the workspace size.
 */
  if( ilon % 2 )  lwork = ilon*(5*jlat1+1);
  else            lwork = 2*ilon*(jlat1+1);
  lsave = 2*(2*jlat+ilon+16);

  lwork = (10*lwork)/9;
  lsave = (10*lsave)/9;

/*
 * Test dimension sizes.
 */
  if((ilon > INT_MAX) || (jlat > INT_MAX) || (jlat1 > INT_MAX) ||
     (lwork > INT_MAX) || (lsave > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  iilon = (int) ilon;
  ijlat = (int) jlat;
  ijlat1 = (int) jlat1;
  ilwork = (int) lwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_goff = index_greg = 0;
  ioff = 0;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_goff != NCL_double) {
/*
 * Coerce jlat x ilon subsection of goff (tmp_goff) to double, if necessary.
 */
      coerce_subset_input_double(goff,tmp_goff,index_goff,type_goff,jlatilon,
                                 0,&missing_goff,&missing_dgoff);
    }
    else {
/*
 * Point tmp_goff and tmp_greg to appropriate locations in goff and greg.
 */
      tmp_goff = &((double*)goff)[index_goff];
      tmp_greg = &((double*)greg)[index_greg];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_goff,jlatilon,has_missing_goff,
                                     missing_dgoff.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(greg,index_greg,type_goff,jlat1ilon,
                                missing_dgoff.doubleval);
    }
    else {
/*
 * Call the f77 version of 'fo2fsh' with the full argument list.
 */
      NGCALLF(dfo2f,DFO2F)(tmp_goff,&iilon,&ijlat,tmp_greg,&ijlat1,
			   work,&ilwork,wsave,&ilsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fsh: ier = %d\n", ier );
      }
/*
 * Copy output values from temporary array "tmp_goff" to final array "goff".
 */
      if(type_greg == NCL_float) {
        coerce_output_float_only(greg,tmp_greg,jlat1ilon,index_greg);
      } 
    }
    index_goff += jlatilon;
    index_greg += jlat1ilon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fsh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if(type_goff != NCL_double) {
    NclFree(tmp_goff);
    NclFree(tmp_greg);
  }
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_goff) {
    if(type_greg == NCL_float) {
      ret = NclReturnValue(greg,ndims_goff,dsizes_greg,&missing_rgoff,
                            type_greg,0);
    }
    else {
      ret = NclReturnValue(greg,ndims_goff,dsizes_greg,&missing_dgoff,
                            type_greg,0);
    }
  }
  else {
    ret = NclReturnValue(greg,ndims_goff,dsizes_greg,NULL,type_greg,0);
  }
  NclFree(dsizes_greg);
  return(ret);
}


NhlErrorTypes f2fosh_W( void )
{
/*
 * Input array variables
 */
  void *greg;
  double *tmp_greg = NULL;
  int ndims_greg;
  ng_size_t jlat, jlat1, ilon;
  ng_size_t dsizes_greg[NCL_MAX_DIMENSIONS];
  NclScalar missing_greg, missing_dgreg, missing_rgreg;
  NclBasicDataTypes type_greg, type_goff;
  int has_missing_greg, found_missing;
/*
 * Output array variables
 */
  void *goff;
  double *tmp_goff = NULL;
  ng_size_t *dsizes_goff;
/*
 * Workspace variables
 */
  ng_size_t lwork, lsave;
  double *work, *wsave;
  int iilon, ijlat, ijlat1, ilwork, ilsave; 
/*
 * error code, various
 */
  ng_size_t i, index_greg, index_goff;
  int ioff, ier = 0, kmiss = 0;
  ng_size_t total_size_leftmost, jlat1ilon, jlatilon;
  ng_size_t total_size_greg, total_size_goff, ret;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  greg = (void*)NclGetArgValue(
           0,
           1,
           &ndims_greg, 
           dsizes_greg,
           &missing_greg,
           &has_missing_greg,
           &type_greg,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_greg < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
/*
 * Compute number of elements.
 */
  compute_jlatilon(dsizes_greg,ndims_greg,&jlat1,&ilon,&jlat1ilon,
                   &jlat,&jlatilon,&total_size_leftmost,&total_size_greg,
                   &total_size_goff,0);
/*
 * Coerce missing values.
 */
  coerce_missing(type_greg,has_missing_greg,&missing_greg,&missing_dgreg,
                 &missing_rgreg);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in goff and greg.
 */
  if(type_greg != NCL_double) {
    type_goff = NCL_float;
    goff = (void*)calloc(total_size_goff,sizeof(float));
    tmp_greg = (double*)calloc(jlat1ilon,sizeof(double));
    tmp_goff = (double*)calloc(jlatilon,sizeof(double));
    if(goff == NULL || tmp_greg == NULL || tmp_goff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: Unable to allocate memory for in/output arrays");
      return(NhlFATAL);
    }
  } 
  else { 
    type_goff = NCL_double;
    goff = (void*)calloc(total_size_goff,sizeof(double));
    if(goff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }

/*
 * Allocate space for dimension sizes and output array.
 */
  dsizes_goff = (ng_size_t *)calloc(ndims_greg,sizeof(ng_size_t));
  if( dsizes_goff == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: Unable to allocate memory for output dimension array");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_greg-2; i++ ) dsizes_goff[i] = dsizes_greg[i];
  dsizes_goff[ndims_greg-2] = jlat;
  dsizes_goff[ndims_greg-1] = ilon;
/*
 * Determine the workspace size.
 */
  if( ilon % 2 )  lwork = ilon*(5*jlat1+1);
  else            lwork = 2*ilon*(jlat1+1);
  lsave = 2*(2*jlat+ilon+16);
  lwork = (10*lwork)/9;
  lsave = (10*lsave)/9;
/*
 * Test dimension sizes.
 */
  if((ilon > INT_MAX) || (jlat > INT_MAX) || (jlat1 > INT_MAX) ||
     (lwork > INT_MAX) || (lsave > INT_MAX)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: one or more dimension sizes is greater than INT_MAX");
        return(NhlFATAL);
  }
  iilon = (int) ilon;
  ijlat = (int) jlat;
  ijlat1 = (int) jlat1;
  ilwork = (int) lwork;
  ilsave = (int) lsave;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_greg = index_goff = 0;
  ioff = 1;
  for(i = 1; i <= total_size_leftmost; i++) {
    if(type_greg != NCL_double) {
/*
 * Coerce jlat1 x ilon subsection of greg (tmp_greg) to double.
 */
      coerce_subset_input_double(greg,tmp_greg,index_greg,type_greg,jlat1ilon,
                                 0,&missing_greg,&missing_dgreg);
    }
    else {
/*
 * Point tmp_goff and tmp_greg to appropriate locations in goff and greg.
 */
      tmp_goff = &((double*)goff)[index_goff];
      tmp_greg = &((double*)greg)[index_greg];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_greg,jlat1ilon,has_missing_greg,
                                     missing_dgreg.doubleval);
    if(found_missing) {
      kmiss++;
/*
 * Set all elements of this 2D grid to a missing value.
 */
      set_subset_output_missing(goff,index_goff,type_greg,jlatilon,
                                missing_dgreg.doubleval);
    }
    else {
/*
 * Call the f77 version of 'f2fosh' with the full argument list.
 */
      NGCALLF(dfo2f,DFO2F)(tmp_goff,&iilon,&ijlat,tmp_greg,&ijlat1,
			   work,&ilwork,wsave,&ilsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fosh: ier = %d\n", ier );
      }
/*
 * Copy output values from temporary array "tmp_greg" to final array "greg".
 */
      if(type_goff == NCL_float) {
        coerce_output_float_only(goff,tmp_goff,jlatilon,index_goff);
      } 
    }
    index_greg += jlat1ilon;
    index_goff += jlatilon;
  }
/*
 * Check if any input arrays had had missing values. If so, print a 
 * warning message.
 */
  if(kmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fosh: %d 2-dimensional input array(s) contained missing values. No interpolation performed on these arrays",kmiss);
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if(type_greg != NCL_double) {
    NclFree(tmp_goff);
    NclFree(tmp_greg);
  }
/*
 * Check if we need to return the missing value attribute.
 */
  if(has_missing_greg) {
    if(type_goff == NCL_float) {
      ret = NclReturnValue(goff,ndims_greg,dsizes_goff,&missing_rgreg,
                            type_goff,0);
    }
    else {
      ret = NclReturnValue(goff,ndims_greg,dsizes_goff,&missing_dgreg,
                            type_goff,0);
    }
  }
  else {
    ret = NclReturnValue(goff,ndims_greg,dsizes_goff,NULL,type_goff,0);
  }
  NclFree(dsizes_goff);
  return(ret);
}

void compute_jlatilon(ng_size_t *dsizes,int ndims,ng_size_t *jlat,ng_size_t *ilon,
                      ng_size_t *jlatilon,ng_size_t *jlat1,ng_size_t *jlat1ilon,ng_size_t *nt,
                      ng_size_t *total_in, ng_size_t *total_out, int iopt)
{
  int i;

  *jlat = dsizes[ndims-2];
  *ilon = dsizes[ndims-1];
  *jlatilon = *jlat * *ilon;
  *nt = 1;
  for(i = 0; i < ndims-2; i++) *nt *= dsizes[i];
  if(iopt) {
    *jlat1 = *jlat + 1;
  }
  else {
    *jlat1 = *jlat - 1;
  }
  *jlat1ilon = *jlat1 * *ilon;

  *total_in  = *jlatilon  * *nt;
  *total_out = *jlat1ilon * *nt;
}


#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(trvsphx,TRVSPHX)(int *,int *,int *,int *,int *,double *,
                                     double *,int *,int *,int *,int *,
                                     double *,double *,double *,int *,
                                     int *,double *,int *,int *,double *,
                                     int *,int *,int *);

extern void NGCALLF(df2foshv,DF2FOSHV)(double *,double *,int *,int *,double *,
                                       double *,int *,double *,int *,double *,
                                       int *,int *,int *);

NhlErrorTypes g2gshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *tmp_Ua = NULL; 
  double *tmp_Va = NULL; 
  int ndims_Ua, ndims_Va;
  ng_size_t dsizes_Ua[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Va[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  int igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *tmp_Ub = NULL; 
  double *tmp_Vb = NULL; 
  int ndims_Ub, ndims_Vb;
  ng_size_t dsizes_Ub[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Vb[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int ier=0;
  int *twave, intl;
  ng_size_t i, index_UVa, index_UVb, l1;
  ng_size_t nlatanlona, nlatbnlonb;
  ng_size_t total_size_in, total_size_out, total_leftmost;
  ng_size_t lsvmin, lwkmin;
  int iveca = 0, ivecb = 0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ildwork, ilsave, ilwork;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (void*)NclGetArgValue(
           0,
           5,
           &ndims_Ua, 
           dsizes_Ua,
           &missing_Ua,
           &has_missing_Ua,
           &type_Ua,
           DONT_CARE);
  Va = (void*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           DONT_CARE);
/* 
 * Get output arrays
 */
  Ub = (void*)NclGetArgValue(
           2,
           5,
           &ndims_Ub, 
           dsizes_Ub,
           NULL,
           NULL,
           &type_Ub,
           1);
  Vb = (void*)NclGetArgValue(
           3,
           5,
           &ndims_Vb, 
           dsizes_Vb,
           NULL,
           NULL,
           &type_Vb,
           1);
/*
 * Get optional wave truncation value.
 */
  twave = (int*)NclGetArgValue(
            4,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_Ua < 2 || ndims_Va < 2 || ndims_Ua != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ua; i++) {
    if( dsizes_Ua[i] != dsizes_Va[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
  if((type_Ub != NCL_float && type_Ub != NCL_double) ||
     (type_Vb != NCL_float && type_Vb != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_Ub != ndims_Ua || ndims_Vb != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ub; i++) {
    if( dsizes_Ub[i] != dsizes_Vb[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_Ub-2 ) {
      if( dsizes_Ub[i] != dsizes_Ua[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in the input arrays.
 */
  compute_nlatanlona(dsizes_Ua,dsizes_Ub,ndims_Ua,ndims_Ub,&nlata,&nlona,
                     &nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_leftmost,&total_size_in,&total_size_out);

/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = 2;
  igrida[1] = igridb[1] = 0;

  intl = 0;
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ua,has_missing_Ua,&missing_Ua,&missing_dUa,NULL);
  coerce_missing(type_Va,has_missing_Va,&missing_Va,&missing_dVa,NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ua != NCL_double) {
    tmp_Ua = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Ua == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for Ua array");
      return(NhlFATAL);
    }
  }
  if(type_Va != NCL_double) {
    tmp_Va = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Va == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for Va array");
      return(NhlFATAL);
    }
  }
  if(type_Ub != NCL_double) {
    tmp_Ub = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Ub == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for Ub array");
      return(NhlFATAL);
    }
  }

  if(type_Vb != NCL_double) {
    tmp_Vb = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Vb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for Vb array");
      return(NhlFATAL);
    }
  }

/*
 * Determine the workspace size.
 */
  la1   = min(nlata,(nlona+2)/2);
  la2   = (nlata+1)/2;
  lb1   = min(nlatb,(nlonb+2)/2);
  lb2   = (nlatb+1)/2;
  lwa   = 4*nlata*la2+3*max(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15; 
  lwb   = 4*nlatb*lb2+3*max(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15; 
  lsave = lwb + lwa; 

  klat  = max(nlata,nlatb);
  klon  = max(nlona,nlonb);
  l1    = min(klat,(klon+2)/2);
  lwork = 2*klat*(8*l1 + 4*klon +3);
  ldwork = 2*klat*(klat+1)+1;

  lsave = (lsave*5)/4;     /* add extra work space */
  lwork = (lwork*5)/4;     /* add extra work space */

/* 
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) ||
     (nlata > INT_MAX) ||
     (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_UVa = index_UVb = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_Ua != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ua (tmp_Ua) to double.
 */
      coerce_subset_input_double(Ua,tmp_Ua,index_UVa,type_Ua,nlatanlona,0,
                                 &missing_Ua,&missing_dUa);
    }
    else {
      tmp_Ua = &((double*)Ua)[index_UVa];
    
    }
    if(type_Va != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Va (tmp_Va) to double.
 */
      coerce_subset_input_double(Va,tmp_Va,index_UVa,type_Va,nlatanlona,0,
                                 &missing_Va,&missing_dVa);
    }
    else {
      tmp_Va = &((double*)Va)[index_UVa];
    }
/*
 * Point output arrays to necessary locations in Ub and Vb if necessary.
 */
    if(type_Ub == NCL_double) tmp_Ub = &((double*)Ub)[index_UVb];
    if(type_Vb == NCL_double) tmp_Vb = &((double*)Vb)[index_UVb];

/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_Ua,nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_Va,nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(Ub,index_UVb,type_Ub,nlatbnlonb,missing);
      set_subset_output_missing(Vb,index_UVb,type_Vb,nlatbnlonb,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(trvsphx,TRVSPHX)(&intl,igrida,&inlona,&inlata,&iveca,tmp_Ua,
			       tmp_Va,igridb,&inlonb,&inlatb,&ivecb,
			       tmp_Ub,tmp_Vb,wsave,&ilsave,&ilsvmin,work,
			       &ilwork,&ilwkmin,dwork,&ildwork,&ier,twave);
      lsvmin = (ng_size_t) ilsvmin;
      lwkmin = (ng_size_t) ilwkmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_U/Vb" to arrays "U/Vb".
 */
      if(type_Ub != NCL_double) {
        coerce_output_float_only(Ub,tmp_Ub,nlatbnlonb,index_UVb);
      } 
      if(type_Vb != NCL_double) {
        coerce_output_float_only(Vb,tmp_Vb,nlatbnlonb,index_UVb);
      } 
    }
    index_UVa += nlatanlona;
    index_UVb += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if(type_Ua != NCL_double) NclFree(tmp_Ua);
  if(type_Va != NCL_double) NclFree(tmp_Va);
  if(type_Ub != NCL_double) NclFree(tmp_Ub);
  if(type_Vb != NCL_double) NclFree(tmp_Vb);
/*
 * Return.
 */
  return(NhlNOERROR);
}


NhlErrorTypes f2gshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *tmp_Ua = NULL; 
  double *tmp_Va = NULL; 
  int ndims_Ua, ndims_Va;
  ng_size_t dsizes_Ua[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Va[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  int igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *tmp_Ub = NULL; 
  double *tmp_Vb = NULL; 
  int ndims_Ub, ndims_Vb;
  ng_size_t dsizes_Ub[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Vb[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int ier=0;
  int *twave, intl;
  ng_size_t i, index_UVa, index_UVb, l1;
  ng_size_t nlatanlona, nlatbnlonb;
  ng_size_t total_size_in, total_size_out, total_leftmost;
  ng_size_t lsvmin, lwkmin;
  int iveca = 0, ivecb = 0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ildwork, ilsave, ilwork;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (void*)NclGetArgValue(
           0,
           5,
           &ndims_Ua, 
           dsizes_Ua,
           &missing_Ua,
           &has_missing_Ua,
           &type_Ua,
           DONT_CARE);
  Va = (void*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           DONT_CARE);
/* 
 * Get output arrays
 */
  Ub = (void*)NclGetArgValue(
           2,
           5,
           &ndims_Ub, 
           dsizes_Ub,
           NULL,
           NULL,
           &type_Ub,
           1);
  Vb = (void*)NclGetArgValue(
           3,
           5,
           &ndims_Vb, 
           dsizes_Vb,
           NULL,
           NULL,
           &type_Vb,
           1);
/*
 * Get optional wave truncation value.
 */
  twave = (int*)NclGetArgValue(
            4,
            5, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_Ua < 2 || ndims_Va < 2 || ndims_Ua != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ua; i++) {
    if( dsizes_Ua[i] != dsizes_Va[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
  if((type_Ub != NCL_float && type_Ub != NCL_double) ||
     (type_Vb != NCL_float && type_Vb != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_Ub != ndims_Ua || ndims_Vb != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ub; i++) {
    if( dsizes_Ub[i] != dsizes_Vb[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_Ub-2 ) {
      if( dsizes_Ub[i] != dsizes_Ua[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in the input arrays.
 */
  compute_nlatanlona(dsizes_Ua,dsizes_Ub,ndims_Ua,ndims_Ub,&nlata,&nlona,
                     &nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_leftmost,&total_size_in,&total_size_out);
/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = 1;
  igridb[0] = 2;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ua,has_missing_Ua,&missing_Ua,&missing_dUa,NULL);
  coerce_missing(type_Va,has_missing_Va,&missing_Va,&missing_dVa,NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ua != NCL_double) {
    tmp_Ua = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Ua == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for Ua array");
      return(NhlFATAL);
    }
  }
  if(type_Va != NCL_double) {
    tmp_Va = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Va == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for Va array");
      return(NhlFATAL);
    }
  }
  if(type_Ub != NCL_double) {
    tmp_Ub = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Ub == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for Ub array");
      return(NhlFATAL);
    }
  }
  if(type_Vb != NCL_double) {
    tmp_Vb = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Vb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for Vb array");
      return(NhlFATAL);
    }
  }

/*
 * Determine the workspace size.
 */
  la1   = min(nlata,(nlona+2)/2);
  la2   = (nlata+1)/2;
  lb1   = min(nlatb,(nlonb+2)/2);
  lb2   = (nlatb+1)/2;
  lwa   = 4*nlata*la2+3*max(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15; 
  lwb   = 4*nlatb*lb2+3*max(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15; 
  lsave = lwb + lwa; 

  klat  = max(nlata,nlatb);
  klon  = max(nlona,nlonb);
  l1    = min(klat,(klon+2)/2);
  lwork = 2*klat*(8*l1 + 4*klon +3);
  ldwork = 2*klat*(klat+1)+1;

  lsave = (lsave*5)/4;     /* add extra work space */
  lwork = (lwork*5)/4;     /* add extra work space */

/* 
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) ||
     (nlata > INT_MAX) ||
     (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_UVa = index_UVb = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_Ua != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ua (tmp_Ua) to double.
 */
      coerce_subset_input_double(Ua,tmp_Ua,index_UVa,type_Ua,nlatanlona,0,
                                 &missing_Ua,&missing_dUa);
    }
    else {
      tmp_Ua = &((double*)Ua)[index_UVa];
    
    }
    if(type_Va != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Va (tmp_Va) to double.
 */
      coerce_subset_input_double(Va,tmp_Va,index_UVa,type_Va,nlatanlona,0,
                                 &missing_Va,&missing_dVa);
    }
    else {
      tmp_Va = &((double*)Va)[index_UVa];
    }
/*
 * Point output arrays to necessary locations in Ub and Vb if necessary.
 */
    if(type_Ub == NCL_double) tmp_Ub = &((double*)Ub)[index_UVb];
    if(type_Vb == NCL_double) tmp_Vb = &((double*)Vb)[index_UVb];
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_Ua,nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_Va,nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(Ub,index_UVb,type_Ub,nlatbnlonb,missing);
      set_subset_output_missing(Vb,index_UVb,type_Vb,nlatbnlonb,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(trvsphx,TRVSPHX)(&intl,igrida,&inlona,&inlata,&iveca,tmp_Ua,
			       tmp_Va,igridb,&inlonb,&inlatb,&ivecb,
			       tmp_Ub,tmp_Vb,wsave,&ilsave,&ilsvmin,work,
			       &ilwork,&ilwkmin,dwork,&ildwork,&ier,twave);
      lsvmin = (ng_size_t) ilsvmin;
      lwkmin = (ng_size_t) ilwkmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_U/Vb" to arrays "U/Vb".
 */
      if(type_Ub != NCL_double) {
        coerce_output_float_only(Ub,tmp_Ub,nlatbnlonb,index_UVb);
      } 
      if(type_Vb != NCL_double) {
        coerce_output_float_only(Vb,tmp_Vb,nlatbnlonb,index_UVb);
      } 
    }
    index_UVa += nlatanlona;
    index_UVb += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if(type_Ua != NCL_double) NclFree(tmp_Ua);
  if(type_Va != NCL_double) NclFree(tmp_Va);
  if(type_Ub != NCL_double) NclFree(tmp_Ub);
  if(type_Vb != NCL_double) NclFree(tmp_Vb);

/*
 * Return.
 */
  return(NhlNOERROR);
}


NhlErrorTypes g2fshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *tmp_Ua = NULL; 
  double *tmp_Va = NULL; 
  int ndims_Ua, ndims_Va;
  ng_size_t dsizes_Ua[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Va[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  int igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *tmp_Ub = NULL; 
  double *tmp_Vb = NULL; 
  int ndims_Ub, ndims_Vb;
  ng_size_t dsizes_Ub[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Vb[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int ier=0;
  int twave = 0, intl;
  ng_size_t i, l1, index_UVa, index_UVb;
  ng_size_t nlatanlona, nlatbnlonb;
  ng_size_t total_size_in, total_size_out, total_leftmost;
  ng_size_t lsvmin, lwkmin;
  int iveca = 0, ivecb = 0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ildwork, ilsave, ilwork;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (void*)NclGetArgValue(
           0,
           4,
           &ndims_Ua, 
           dsizes_Ua,
           &missing_Ua,
           &has_missing_Ua,
           &type_Ua,
           DONT_CARE);
  Va = (void*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           DONT_CARE);
/* 
 * Get output arrays
 */
  Ub = (void*)NclGetArgValue(
           2,
           4,
           &ndims_Ub, 
           dsizes_Ub,
           NULL,
           NULL,
           &type_Ub,
           1);
  Vb = (void*)NclGetArgValue(
           3,
           4,
           &ndims_Vb, 
           dsizes_Vb,
           NULL,
           NULL,
           &type_Vb,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_Ua < 2 || ndims_Va < 2 || ndims_Ua != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ua; i++) {
    if( dsizes_Ua[i] != dsizes_Va[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
  if((type_Ub != NCL_float && type_Ub != NCL_double) ||
     (type_Vb != NCL_float && type_Vb != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_Ub != ndims_Ua || ndims_Vb != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ub; i++) {
    if( dsizes_Ub[i] != dsizes_Vb[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_Ub-2 ) {
      if( dsizes_Ub[i] != dsizes_Ua[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in the input arrays.
 */
  compute_nlatanlona(dsizes_Ua,dsizes_Ub,ndims_Ua,ndims_Ub,&nlata,&nlona,
                     &nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_leftmost,&total_size_in,&total_size_out);
/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = 2;
  igridb[0] = 1;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ua,has_missing_Ua,&missing_Ua,&missing_dUa,NULL);
  coerce_missing(type_Va,has_missing_Va,&missing_Va,&missing_dVa,NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ua != NCL_double) {
    tmp_Ua = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Ua == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Unable to allocate memory for input Ua array");
      return(NhlFATAL);
    }
  } 
  if(type_Va != NCL_double) {
    tmp_Va = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Va == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Unable to allocate memory for input Va array");
      return(NhlFATAL);
    }
  } 
  if(type_Ub != NCL_double) {
    tmp_Ub = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Ub == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Unable to allocate memory for input Ub array");
      return(NhlFATAL);
    }
  } 
  if(type_Vb != NCL_double) {
    tmp_Vb = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Vb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Bnable to allocate memory for input Vb array");
      return(NhlFATAL);
    }
  } 
/*
 * Determine the workspace size.
 */
  la1   = min(nlata,(nlona+2)/2);
  la2   = (nlata+1)/2;
  lb1   = min(nlatb,(nlonb+2)/2);
  lb2   = (nlatb+1)/2;
  lwa   = 4*nlata*la2+3*max(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15; 
  lwb   = 4*nlatb*lb2+3*max(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15; 
  lsave = lwb + lwa; 

  klat  = max(nlata,nlatb);
  klon  = max(nlona,nlonb);
  l1    = min(klat,(klon+2)/2);
  lwork = 2*klat*(8*l1 + 4*klon +3);
  ldwork = 2*klat*(klat+1)+1;

  lsave = (lsave*5)/4;     /* add extra work space */
  lwork = (lwork*5)/4;     /* add extra work space */

/* 
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) ||
     (nlata > INT_MAX) ||
     (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_UVa = index_UVb = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_Ua != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ua (tmp_Ua) to double.
 */
      coerce_subset_input_double(Ua,tmp_Ua,index_UVa,type_Ua,nlatanlona,0,
                                 &missing_Ua,&missing_dUa);
    }
    else {
      tmp_Ua = &((double*)Ua)[index_UVa];
    
    }
    if(type_Va != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Va (tmp_Va) to double.
 */
      coerce_subset_input_double(Va,tmp_Va,index_UVa,type_Va,nlatanlona,0,
                                 &missing_Va,&missing_dVa);
    }
    else {
      tmp_Va = &((double*)Va)[index_UVa];
    }
/*
 * Point output arrays to necessary locations in Ub and Vb if necessary.
 */
    if(type_Ub == NCL_double) tmp_Ub = &((double*)Ub)[index_UVb];
    if(type_Vb == NCL_double) tmp_Vb = &((double*)Vb)[index_UVb];
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_Ua,nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_Va,nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(Ub,index_UVb,type_Ub,nlatbnlonb,missing);
      set_subset_output_missing(Vb,index_UVb,type_Vb,nlatbnlonb,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(trvsphx,TRVSPHX)(&intl,igrida,&inlona,&inlata,&iveca,tmp_Ua,
			       tmp_Va,igridb,&inlonb,&inlatb,&ivecb,
			       tmp_Ub,tmp_Vb,wsave,&ilsave,&ilsvmin,work,
			       &ilwork,&ilwkmin,dwork,&ildwork,&ier,&twave);
      lsvmin = (ng_size_t) ilsvmin;
      lwkmin = (ng_size_t) ilwkmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_U/Vb" to arrays "U/Vb".
 */
      if(type_Ub != NCL_double) {
        coerce_output_float_only(Ub,tmp_Ub,nlatbnlonb,index_UVb);
      } 
      if(type_Vb != NCL_double) {
        coerce_output_float_only(Vb,tmp_Vb,nlatbnlonb,index_UVb);
      } 
    }
    index_UVa += nlatanlona;
    index_UVb += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if(type_Ua != NCL_double) NclFree(tmp_Ua);
  if(type_Va != NCL_double) NclFree(tmp_Va);
  if(type_Ub != NCL_double) NclFree(tmp_Ub);
  if(type_Vb != NCL_double) NclFree(tmp_Vb);
/*
 * Return.
 */
  return(NhlNOERROR);
}


NhlErrorTypes f2fshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *tmp_Ua = NULL; 
  double *tmp_Va = NULL; 
  int ndims_Ua, ndims_Va;
  ng_size_t dsizes_Ua[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Va[NCL_MAX_DIMENSIONS];
  ng_size_t nlata, nlona;
  int igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *tmp_Ub = NULL;
  double *tmp_Vb = NULL;
  int ndims_Ub, ndims_Vb;
  ng_size_t dsizes_Ub[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_Vb[NCL_MAX_DIMENSIONS];
  ng_size_t nlatb, nlonb;
  int igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int ier=0;
  int twave = 0, intl;
  ng_size_t i, l1, index_UVa, index_UVb;
  ng_size_t nlatanlona, nlatbnlonb;
  ng_size_t total_size_in, total_size_out, total_leftmost;
  ng_size_t lsvmin, lwkmin;
  int iveca = 0, ivecb = 0;
/*
 * Workspace variables
 */
  ng_size_t lsave, lwork, ldwork;
  ng_size_t klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
  int inlona, inlata, inlonb, inlatb, ildwork, ilsave, ilwork;
  int ilsvmin, ilwkmin;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (void*)NclGetArgValue(
           0,
           4,
           &ndims_Ua, 
           dsizes_Ua,
           &missing_Ua,
           &has_missing_Ua,
           &type_Ua,
           DONT_CARE);
  Va = (void*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           DONT_CARE);
/* 
 * Get output arrays
 */
  Ub = (void*)NclGetArgValue(
           2,
           4,
           &ndims_Ub, 
           dsizes_Ub,
           NULL,
           NULL,
           &type_Ub,
           1);
  Vb = (void*)NclGetArgValue(
           3,
           4,
           &ndims_Vb, 
           dsizes_Vb,
           NULL,
           NULL,
           &type_Vb,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_Ua < 2 || ndims_Va < 2 || ndims_Ua != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ua; i++) {
    if( dsizes_Ua[i] != dsizes_Va[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
  if((type_Ub != NCL_float && type_Ub != NCL_double) ||
     (type_Vb != NCL_float && type_Vb != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_Ub != ndims_Ua || ndims_Vb != ndims_Va ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_Ub; i++) {
    if( dsizes_Ub[i] != dsizes_Vb[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_Ub-2 ) {
      if( dsizes_Ub[i] != dsizes_Ua[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
/*
 * Compute the total number of elements in the input arrays.
 */
  compute_nlatanlona(dsizes_Ua,dsizes_Ub,ndims_Ua,ndims_Ub,&nlata,&nlona,
                     &nlatanlona,&nlatb,&nlonb,&nlatbnlonb,
                     &total_leftmost,&total_size_in,&total_size_out);
/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = 1;
  igrida[1] = igridb[1] = 0;

  intl = 0;
/*
 * Coerce missing values.
 */
  coerce_missing(type_Ua,has_missing_Ua,&missing_Ua,&missing_dUa,NULL);
  coerce_missing(type_Va,has_missing_Va,&missing_Va,&missing_dVa,NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in Ta and Tb.
 */
  if(type_Ua != NCL_double) {
    tmp_Ua = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Ua == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for Ua array");
      return(NhlFATAL);
    }
  } 
  if(type_Va != NCL_double) {
    tmp_Va = (double*)calloc(nlatanlona,sizeof(double));
    if(tmp_Va == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for Va array");
      return(NhlFATAL);
    }
  } 
  if(type_Ub != NCL_double) {
    tmp_Ub = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Ub == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for Ub array");
      return(NhlFATAL);
    }
  } 
  if(type_Vb != NCL_double) {
    tmp_Vb = (double*)calloc(nlatbnlonb,sizeof(double));
    if(tmp_Vb == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for Vb array");
      return(NhlFATAL);
    }
  } 
/*
 * Determine the workspace size.
 */
  la1   = min(nlata,(nlona+2)/2);
  la2   = (nlata+1)/2;
  lb1   = min(nlatb,(nlonb+2)/2);
  lb2   = (nlatb+1)/2;
  lwa   = 4*nlata*la2+3*max(la1-2,0)*(2*nlata-la1-1)+la2+nlona+15; 
  lwb   = 4*nlatb*lb2+3*max(lb1-2,0)*(2*nlatb-lb1-1)+nlonb+15; 
  lsave = lwb + lwa; 

  klat  = max(nlata,nlatb);
  klon  = max(nlona,nlonb);
  l1    = min(klat,(klon+2)/2);
  lwork = 2*klat*(8*l1 + 4*klon +3);
  ldwork = 2*klat*(klat+1)+1;

  lsave = (lsave*5)/4;     /* add extra work space */
  lwork = (lwork*5)/4;     /* add extra work space */

/* 
 * Test dimension sizes.
 */
  if((nlona > INT_MAX) ||
     (nlata > INT_MAX) ||
     (nlonb > INT_MAX) ||
     (nlatb > INT_MAX) ||
     (ldwork > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlona = (int) nlona;
  inlata = (int) nlata;
  inlonb = (int) nlonb;
  inlatb = (int) nlatb;
  ildwork = (int) ldwork;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  dwork = (double *)calloc(ldwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_UVa = index_UVb = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_Ua != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Ua (tmp_Ua) to double.
 */
      coerce_subset_input_double(Ua,tmp_Ua,index_UVa,type_Ua,nlatanlona,0,
                                 &missing_Ua,&missing_dUa);
    }
    else {
      tmp_Ua = &((double*)Ua)[index_UVa];
    
    }
    if(type_Va != NCL_double) {
/*
 * Coerce nlata x nlona subsection of Va (tmp_Va) to double.
 */
      coerce_subset_input_double(Va,tmp_Va,index_UVa,type_Va,nlatanlona,0,
                                 &missing_Va,&missing_dVa);
    }
    else {
      tmp_Va = &((double*)Va)[index_UVa];
    }
/*
 * Point output arrays to necessary locations in Ub and Vb if necessary.
 */
    if(type_Ub == NCL_double) tmp_Ub = &((double*)Ub)[index_UVb];
    if(type_Vb == NCL_double) tmp_Vb = &((double*)Vb)[index_UVb];

/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_Ua,nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_Va,nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(Ub,index_UVb,type_Ub,nlatbnlonb,missing);
      set_subset_output_missing(Vb,index_UVb,type_Vb,nlatbnlonb,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(trvsphx,TRVSPHX)(&intl,igrida,&inlona,&inlata,&iveca,tmp_Ua,
			       tmp_Va,igridb,&inlonb,&inlatb,&ivecb,
			       tmp_Ub,tmp_Vb,wsave,&ilsave,&ilsvmin,work,
			       &ilwork,&ilwkmin,dwork,&ildwork,&ier,&twave);
      lsvmin = (ng_size_t) ilsvmin;
      lwkmin = (ng_size_t) ilwkmin;

      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_U/Vb" to arrays "U/Vb".
 */
      if(type_Ub != NCL_double) {
        coerce_output_float_only(Ub,tmp_Ub,nlatbnlonb,index_UVb);
      } 
      if(type_Vb != NCL_double) {
        coerce_output_float_only(Vb,tmp_Vb,nlatbnlonb,index_UVb);
      } 
    }
    index_UVa += nlatanlona;
    index_UVb += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if(type_Ua != NCL_double) NclFree(tmp_Ua);
  if(type_Va != NCL_double) NclFree(tmp_Va);
  if(type_Ub != NCL_double) NclFree(tmp_Ub);
  if(type_Vb != NCL_double) NclFree(tmp_Vb);
/*
 * Return.
 */
  return(NhlNOERROR);
}

NhlErrorTypes fo2fshv_W( void )
{
/*
 * Input array variables
 */
  void *uoff, *voff;
  double *tmp_uoff = NULL;
  double *tmp_voff = NULL;
  int ndims_uoff, ndims_voff;
  ng_size_t dsizes_uoff[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_voff[NCL_MAX_DIMENSIONS];
  ng_size_t jlat, jlat1, ilon, ilon2, jlatilon,jlat1ilon;
  NclScalar missing_uoff, missing_voff, missing_duoff, missing_dvoff;
  int has_missing_uoff, has_missing_voff, found_missing;
  NclBasicDataTypes type_uoff, type_voff;
  double missing;
/*
 * Output array variables
 */
  void *ureg, *vreg;
  double *tmp_ureg = NULL;
  double *tmp_vreg = NULL;
  int ndims_ureg, ndims_vreg;
  ng_size_t dsizes_ureg[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_vreg[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ureg, type_vreg;
/*
 * various
 */
  int ioff, ier=0;
  ng_size_t i, index_uvoff, index_uvreg;
  ng_size_t total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  ng_size_t lsave, lwork;
  double *work, *wsave;
  int iilon, ijlat, ijlat1, ilsave, ilwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  uoff = (void*)NclGetArgValue(
           0,
           4,
           &ndims_uoff, 
           dsizes_uoff,
           &missing_uoff,
           &has_missing_uoff,
           &type_uoff,
           DONT_CARE);
  voff = (void*)NclGetArgValue(
           1,
           4,
           &ndims_voff, 
           dsizes_voff,
           &missing_voff,
           &has_missing_voff,
           &type_voff,
           DONT_CARE);
/* 
 * Get output arrays
 */
  ureg = (void*)NclGetArgValue(
           2,
           4,
           &ndims_ureg, 
           dsizes_ureg,
           NULL,
           NULL,
           &type_ureg,
           1);
  vreg = (void*)NclGetArgValue(
           3,
           4,
           &ndims_vreg, 
           dsizes_vreg,
           NULL,
           NULL,
           &type_vreg,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_uoff < 2 || ndims_voff < 2 || ndims_uoff != ndims_voff ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_uoff; i++) {
    if( dsizes_uoff[i] != dsizes_voff[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
/*
 * Get dimension sizes and compute total sizes of arrays.
 */
  compute_nlatanlona(dsizes_uoff,dsizes_ureg,ndims_uoff,ndims_ureg,
                     &jlat,&ilon,&jlatilon,&jlat1,&ilon2,&jlat1ilon,
                     &total_leftmost,&total_size_in,&total_size_out);

  if(jlat1 != jlat + 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The last two dimensions of the output arrays must be (jlat+1) x ilon where jlat and ilon are the last two dimensions of the input arrays");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_ureg != ndims_uoff || ndims_vreg != ndims_voff ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  if(dsizes_ureg[ndims_uoff-1] != ilon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The last two dimensions of the output arrays must be (jlat+1) x ilon where jlat and ilon are the last two dimensions of the input arrays");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_ureg; i++) {
    if( dsizes_ureg[i] != dsizes_vreg[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_ureg-2 ) {
      if( dsizes_ureg[i] != dsizes_uoff[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
  if((type_ureg != NCL_float && type_ureg != NCL_double) ||
     (type_vreg != NCL_float && type_vreg != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_uoff,has_missing_uoff,&missing_uoff,&missing_duoff,
                 NULL);
  coerce_missing(type_voff,has_missing_voff,&missing_voff,&missing_dvoff,
                 NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in uoff/voff and ureg/vreg.
 */
  if(type_uoff != NCL_double) {
    tmp_uoff = (double*)calloc(jlatilon,sizeof(double));
    if(tmp_uoff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for uoff array");
      return(NhlFATAL);
    }
  } 
  if(type_voff != NCL_double) {
    tmp_voff = (double*)calloc(jlatilon,sizeof(double));
    if(tmp_voff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for voff array");
      return(NhlFATAL);
    }
  } 
  if(type_ureg != NCL_double) {
    tmp_ureg = (double*)calloc(jlat1ilon,sizeof(double));
    if(tmp_ureg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for ureg array");
      return(NhlFATAL);
    }
  } 
  if(type_vreg != NCL_double) {
    tmp_vreg = (double*)calloc(jlat1ilon,sizeof(double));
    if(tmp_vreg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for vreg array");
      return(NhlFATAL);
    }
  } 
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
  if((ilon > INT_MAX) ||
     (jlat > INT_MAX) ||
     (jlat1 > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  iilon = (int) ilon;
  ijlat = (int) jlat;
  ijlat1 = (int) jlat1;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
  ioff = 0;
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_uvoff = index_uvreg = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_uoff != NCL_double) {
/*
 * Coerce jlat x ilon subsection of uoff (tmp_uoff) to double.
 */
      coerce_subset_input_double(uoff,tmp_uoff,index_uvoff,type_uoff,jlatilon,
                                 0,&missing_uoff,&missing_duoff);
    }
    else {
      tmp_uoff = &((double*)uoff)[index_uvoff];
    
    }
    if(type_voff != NCL_double) {
/*
 * Coerce jlat x ilon subsection of voff (tmp_voff) to double.
 */
      coerce_subset_input_double(voff,tmp_voff,index_uvoff,type_voff,jlatilon,
                                 0,&missing_voff,&missing_dvoff);
    }
    else {
      tmp_voff = &((double*)voff)[index_uvoff];
    }
/*
 * Point output arrays to necessary locations in ureg and vreg if necessary.
 */
    if(type_ureg == NCL_double) tmp_ureg = &((double*)ureg)[index_uvreg];
    if(type_vreg == NCL_double) tmp_vreg = &((double*)vreg)[index_uvreg];
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_uoff,jlatilon,has_missing_uoff,
                                     missing_duoff.doubleval);
    if(found_missing) {
      missing = missing_duoff.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_voff,jlatilon,has_missing_voff,
                                       missing_dvoff.doubleval);
      if(found_missing) missing = missing_dvoff.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(ureg,index_uvreg,type_ureg,jlat1ilon,missing);
      set_subset_output_missing(ureg,index_uvreg,type_vreg,jlat1ilon,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
      NGCALLF(df2foshv,DF2FOSHV)(tmp_uoff,tmp_voff,&iilon,&ijlat,
				 tmp_ureg,tmp_vreg,&ijlat1,
				 work,&ilwork,wsave,&ilsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_u/vreg" to final array "u/vreg".
 */
      if(type_ureg != NCL_double) {
        coerce_output_float_only(ureg,tmp_ureg,jlat1ilon,index_uvreg);
      } 
      if(type_vreg != NCL_double) {
        coerce_output_float_only(vreg,tmp_vreg,jlat1ilon,index_uvreg);
      } 
    }
    index_uvoff += jlatilon;
    index_uvreg += jlat1ilon;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if(type_uoff != NCL_double) NclFree(tmp_uoff);
  if(type_voff != NCL_double) NclFree(tmp_voff);
  if(type_ureg != NCL_double) NclFree(tmp_ureg);
  if(type_vreg != NCL_double) NclFree(tmp_vreg);

/*
 * Return.
 */
  return(NhlNOERROR);
}


NhlErrorTypes f2foshv_W( void )
{
/*
 * Input array vaiables
 */
  void *ureg, *vreg;
  double *tmp_ureg = NULL;
  double *tmp_vreg = NULL;
  int ndims_ureg, ndims_vreg;
  ng_size_t dsizes_ureg[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_vreg[NCL_MAX_DIMENSIONS];
  ng_size_t jlat, jlat1, ilon, ilon2, jlatilon, jlat1ilon;
  NclScalar missing_ureg, missing_vreg, missing_dureg, missing_dvreg;
  int has_missing_ureg, has_missing_vreg, found_missing;
  NclBasicDataTypes type_ureg, type_vreg;
  double missing;
/*
 * Output array variables
 */
  void *uoff, *voff;
  double *tmp_uoff = NULL;
  double *tmp_voff = NULL;
  int ndims_uoff, ndims_voff;
  ng_size_t dsizes_uoff[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_voff[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_uoff, type_voff;
/*
 * various
 */
  int ioff, ier=0;
  ng_size_t i, index_uvreg, index_uvoff;
  ng_size_t total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork;
  double *work, *wsave;
  int iilon, ijlat, ijlat1, ilsave, ilwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ureg = (void*)NclGetArgValue(
           0,
           4,
           &ndims_ureg, 
           dsizes_ureg,
           &missing_ureg,
           &has_missing_ureg,
           &type_ureg,
           DONT_CARE);
  vreg = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vreg, 
           dsizes_vreg,
           &missing_vreg,
           &has_missing_vreg,
           &type_vreg,
           DONT_CARE);
/* 
 * Get output arrays
 */
  uoff = (void*)NclGetArgValue(
           2,
           4,
           &ndims_uoff, 
           dsizes_uoff,
           NULL,
           NULL,
           &type_uoff,
           1);
  voff = (void*)NclGetArgValue(
           3,
           4,
           &ndims_voff, 
           dsizes_voff,
           NULL,
           NULL,
           &type_voff,
           1);
/*
 * The grids coming in must be at least 2-dimensional and have the same
 * size.
 */
  if( ndims_ureg < 2 || ndims_vreg < 2 || ndims_ureg != ndims_vreg ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The input arrays must be at least a 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_ureg; i++) {
    if( dsizes_ureg[i] != dsizes_vreg[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }
/*
 * Compute the total number of elements in the input arrays.
 */
/*
 * Get dimension sizes and compute total sizes of arrays.
 */
  compute_nlatanlona(dsizes_ureg,dsizes_uoff,ndims_ureg,ndims_uoff,
                     &jlat1,&ilon,&jlat1ilon,&jlat,&ilon2,&jlatilon,
                     &total_leftmost,&total_size_in,&total_size_out);

  if((jlat1-1) != jlat) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The last two dimensions of the output arrays must be jlat1 x ilon where jlat1-1 and ilon are the last two dimensions of the input arrays");
    return(NhlFATAL);
  }
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_uoff != ndims_ureg || ndims_voff != ndims_vreg ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  if(dsizes_uoff[ndims_ureg-1] != ilon) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The last two dimensions of the output arrays must be (jlat1-1) x ilon where jlat1 and ilon are the last two dimensions of the input arrays");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_uoff; i++) {
    if( dsizes_uoff[i] != dsizes_voff[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The output arrays must have the same dimensions");
      return(NhlFATAL);
    }
    if( i < ndims_uoff-2 ) {
      if( dsizes_uoff[i] != dsizes_ureg[i] ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: All but the last two dimensions of the output arrays must have the same dimensions as the input arrays");
        return(NhlFATAL);
      }
    }
  }
  if((type_uoff != NCL_float && type_uoff != NCL_double) ||
     (type_voff != NCL_float && type_voff != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The output arrays must be float or double");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_ureg,has_missing_ureg,&missing_ureg,&missing_dureg,
                 NULL);
  coerce_missing(type_vreg,has_missing_vreg,&missing_vreg,&missing_dvreg,
                 NULL);
/*
 * Allocate space for temporary input and output. If the input is not double,
 * then the output will be float, otherwise the output will be double.
 * The temporary arrays are just big enough to hold a 2-dimensional subsection
 * of the input and output. We only need to allocate space for them if the
 * input is not already double, otherwise, we just have them point to the
 * appropriate locations in uoff/voff and ureg/vreg.
 */
  if(type_uoff != NCL_double) {
    tmp_uoff = (double*)calloc(jlatilon,sizeof(double));
    if(tmp_uoff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for uoff array");
      return(NhlFATAL);
    }
  } 

  if(type_voff != NCL_double) {
    tmp_voff = (double*)calloc(jlatilon,sizeof(double));
    if(tmp_voff == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for voff array");
      return(NhlFATAL);
    }
  } 
  if(type_ureg != NCL_double) {
    tmp_ureg = (double*)calloc(jlat1ilon,sizeof(double));
    if(tmp_ureg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for ureg array");
      return(NhlFATAL);
    }
  } 
  if(type_vreg != NCL_double) {
    tmp_vreg = (double*)calloc(jlat1ilon,sizeof(double));
    if(tmp_vreg == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for vreg array");
      return(NhlFATAL);
    }
  } 

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
  if((ilon > INT_MAX) ||
     (jlat > INT_MAX) ||
     (jlat1 > INT_MAX) ||
     (lsave > INT_MAX) ||
     (lwork > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  iilon = (int) ilon;
  ijlat = (int) jlat;
  ijlat1 = (int) jlat1;
  ilsave = (int) lsave;
  ilwork = (int) lwork;

/*
 * Dynamically allocate the various work space.
 */
  work  = (double *)calloc(lwork,sizeof(double));
  wsave = (double *)calloc(lsave,sizeof(double));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
  ioff = 1;
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  index_uvreg = index_uvoff = 0;
  for(i = 1; i <= total_leftmost; i++) {
    if(type_ureg != NCL_double) {
/*
 * Coerce jlat1 x ilon subsection of ureg (tmp_ureg) to double.
 */
      coerce_subset_input_double(ureg,tmp_ureg,index_uvreg,type_ureg,
                                 jlat1ilon,0,&missing_ureg,&missing_dureg);
    }
    else {
      tmp_ureg = &((double*)ureg)[index_uvreg];
    
    }
    if(type_vreg != NCL_double) {
/*
 * Coerce jlat1 x ilon subsection of vreg (tmp_vreg) to double.
 */
      coerce_subset_input_double(vreg,tmp_vreg,index_uvreg,type_vreg,
                                 jlat1ilon,0,&missing_vreg,&missing_dvreg);
    }
    else {
      tmp_vreg = &((double*)vreg)[index_uvreg];
    }
/*
 * Point output arrays to necessary locations in uoff and voff if necessary.
 */
    if(type_uoff == NCL_double) tmp_uoff = &((double*)uoff)[index_uvoff];
    if(type_voff == NCL_double) tmp_voff = &((double*)voff)[index_uvoff];
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(tmp_ureg,jlat1ilon,has_missing_ureg,
                                     missing_dureg.doubleval);
    if(found_missing) {
      missing = missing_dureg.doubleval;
    }
    else {
      found_missing = contains_missing(tmp_vreg,jlat1ilon,has_missing_vreg,
                                       missing_dvreg.doubleval);
      if(found_missing) missing = missing_dvreg.doubleval;
    }
    if(found_missing) {
      set_subset_output_missing(uoff,index_uvoff,type_uoff,jlatilon,missing);
      set_subset_output_missing(uoff,index_uvoff,type_voff,jlatilon,missing);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
      NGCALLF(df2foshv,DF2FOSHV)(tmp_uoff,tmp_voff,&iilon,&ijlat,
				 tmp_ureg,tmp_vreg,&ijlat1,
				 work,&ilwork,wsave,&ilsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: ier = %d\n", ier );
      }
/*
 * Copy output values from temp arrays "tmp_u/voff" to final array "u/voff".
 */
      if(type_uoff != NCL_double) {
        coerce_output_float_only(uoff,tmp_uoff,jlatilon,index_uvoff);
      } 
      if(type_voff != NCL_double) {
        coerce_output_float_only(voff,tmp_voff,jlatilon,index_uvoff);
      } 
    }
    index_uvoff += jlatilon;
    index_uvreg += jlat1ilon;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if(type_uoff != NCL_double) NclFree(tmp_uoff);
  if(type_voff != NCL_double) NclFree(tmp_voff);
  if(type_ureg != NCL_double) NclFree(tmp_ureg);
  if(type_vreg != NCL_double) NclFree(tmp_vreg);

/*
 * Return.
 */
  return(NhlNOERROR);
}


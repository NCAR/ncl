#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include "wrapper.h"

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

extern void NGCALLF(dtrvsph77,DTRVSPH77)(int *,int *,int *,int *,double *,
                                         double *,int *,int *,int *,double *,
                                         double *,int *,double *,int *,
                                         double *,int *,double *,int *,int *);

extern void NGCALLF(df2foshv,DF2FOSHV)(double *,double *,int *,int *,double *,
                                       double *,int *,double *,int *,double *,
                                       int *,int *,int *);

NhlErrorTypes g2gshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *dUa, *dVa;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *dUb, *dVb;
  float *rUb, *rVb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, l1, ier=0;
  int nlatanlona, nlatbnlonb;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
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
           2);
  Va = (void*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           2);
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
            2);
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
  igrida[0] = igridb[0] = -2;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/* 
 * Coerce Ua and Va to double.
 */
  dUa = coerce_input_double(Ua,type_Ua,total_size_in,has_missing_Ua,
                            &missing_Ua,&missing_dUa,NULL);
  dVa = coerce_input_double(Va,type_Va,total_size_in,has_missing_Va,
                            &missing_Va,&missing_dVa,NULL);
  if(dUa == NULL || dVa == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure Ub and Vb are double.
 */
  dUb = coerce_output_double(Ub,type_Ub,total_size_out);
  dVb = coerce_output_double(Vb,type_Vb,total_size_out);

  if(dUb == NULL || dVb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gshv: Unable to allocate memory for converting output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&dUa[lin],nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(&dVa[lin],nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < nlatbnlonb; j++) {
        dUb[lout+j] = missing;
        dVb[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(dtrvsph77,DTRVSPH77)(&intl,igrida,&nlona,&nlata,
                                   &dUa[lin],&dVa[lin],
                                   igridb,&nlonb,&nlatb,&dUb[lout],&dVb[lout],
                                   twave,work,&lwork,wsave,&lsave,dwork,
                                   &ldwork,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: ier = %d\n", ier );
      }
    }
    lin  += nlatanlona;
    lout += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if((void*)dUa != Ua) NclFree(dUa);
  if((void*)dVa != Va) NclFree(dVa);
/*
 * Return.
 */
  if(type_Ub == NCL_float) rUb = coerce_output_float(dUb,Ub,total_size_out,1);
  if(type_Vb == NCL_float) rVb = coerce_output_float(dVb,Vb,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes f2gshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *dUa, *dVa;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *dUb, *dVb;
  float *rUb, *rVb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, l1, ier=0;
  int nlatanlona, nlatbnlonb;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
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
           2);
  Va = (void*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           2);
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
            2);
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
  igrida[0] = -1;
  igridb[0] = -2;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/*
 * coerce Ua and Va to double.
 */
  dUa = coerce_input_double(Ua,type_Ua,total_size_in,has_missing_Ua,
                            &missing_Ua,&missing_dUa,NULL);
  dVa = coerce_input_double(Va,type_Va,total_size_in,has_missing_Va,
                            &missing_Va,&missing_dVa,NULL);
  if(dUa == NULL || dVa == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure Ub and Vb are double.
 */
  dUb = coerce_output_double(Ub,type_Ub,total_size_out);
  dVb = coerce_output_double(Vb,type_Vb,total_size_out);

  if(dUb == NULL || dVb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gshv: Unable to allocate memory for converting output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&dUa[lin],nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(&dVa[lin],nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < nlatbnlonb; j++) {
        dUb[lout+j] = missing;
        dVb[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(dtrvsph77,DTRVSPH77)(&intl,igrida,&nlona,&nlata,
                                   &dUa[lin],&dVa[lin],
                                   igridb,&nlonb,&nlatb,&dUb[lout],&dVb[lout],
                                   twave,work,&lwork,wsave,&lsave,dwork,
                                   &ldwork,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: ier = %d\n", ier );
      }
    }
    lin  += nlatanlona;
    lout += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if((void*)dUa != Ua) NclFree(dUa);
  if((void*)dVa != Va) NclFree(dVa);

/*
 * Return.
 */
  if(type_Ub == NCL_float) rUb = coerce_output_float(dUb,Ub,total_size_out,1);
  if(type_Vb == NCL_float) rVb = coerce_output_float(dVb,Vb,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes g2fshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *dUa, *dVa;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *dUb, *dVb;
  float *rUb, *rVb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int twave = 0, intl, i, j, l1, lin, lout, ier=0;
  int nlatanlona, nlatbnlonb;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
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
           2);
  Va = (void*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           2);
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
  igrida[0] = -2;
  igridb[0] = -1;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/*
 * coerce Ua and Va to double.
 */
  dUa = coerce_input_double(Ua,type_Ua,total_size_in,has_missing_Ua,
                            &missing_Ua,&missing_dUa,NULL);
  dVa = coerce_input_double(Va,type_Va,total_size_in,has_missing_Va,
                            &missing_Va,&missing_dVa,NULL);
  if(dUa == NULL || dVa == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure Ub and Vb are double.
 */
  dUb = coerce_output_double(Ub,type_Ub,total_size_out);
  dVb = coerce_output_double(Vb,type_Vb,total_size_out);

  if(dUb == NULL || dVb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fshv: Unable to allocate memory for converting output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&dUa[lin],nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(&dVa[lin],nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < nlatbnlonb; j++) {
        dUb[lout+j] = missing;
        dVb[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(dtrvsph77,DTRVSPH77)(&intl,igrida,&nlona,&nlata,
                                   &dUa[lin],&dVa[lin],
                                   igridb,&nlonb,&nlatb,&dUb[lout],&dVb[lout],
                                   &twave,work,&lwork,wsave,&lsave,dwork,
                                   &ldwork,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: ier = %d\n", ier );
      }
    }
    lin  += nlatanlona;
    lout += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if((void*)dUa != Ua) NclFree(dUa);
  if((void*)dVa != Va) NclFree(dVa);
/*
 * Return.
 */
  if(type_Ub == NCL_float) rUb = coerce_output_float(dUb,Ub,total_size_out,1);
  if(type_Vb == NCL_float) rVb = coerce_output_float(dVb,Vb,total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes f2fshv_W( void )
{
/*
 * Input array variables
 */
  void *Ua, *Va;
  double *dUa, *dVa;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va, missing_dUa, missing_dVa;
  int has_missing_Ua, has_missing_Va, found_missing;
  NclBasicDataTypes type_Ua, type_Va;
  double missing;
/*
 * Output array variables
 */
  void *Ub, *Vb;
  double *dUb, *dVb;
  float *rUb, *rVb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
  NclBasicDataTypes type_Ub, type_Vb;
/*
 * various
 */
  int twave = 0, intl, i, j, l1, lin, lout, ier=0;
  int nlatanlona, nlatbnlonb;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  double *work, *wsave, *dwork;
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
           2);
  Va = (void*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
           &missing_Va,
           &has_missing_Va,
           &type_Va,
           2);
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
  igrida[0] = igridb[0] = -1;
  igrida[1] = igridb[1] =  0;

  intl = 0;
/*
 * coerce Ua and Va to double.
 */
  dUa = coerce_input_double(Ua,type_Ua,total_size_in,has_missing_Ua,
                            &missing_Ua,&missing_dUa,NULL);
  dVa = coerce_input_double(Va,type_Va,total_size_in,has_missing_Va,
                            &missing_Va,&missing_dVa,NULL);
  if(dUa == NULL || dVa == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure Ub and Vb are double.
 */
  dUb = coerce_output_double(Ub,type_Ub,total_size_out);
  dVb = coerce_output_double(Vb,type_Vb,total_size_out);

  if(dUb == NULL || dVb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: Unable to allocate memory for converting output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&dUa[lin],nlatanlona,has_missing_Ua,
                                     missing_dUa.doubleval);
    if(found_missing) {
      missing = missing_dUa.doubleval;
    }
    else {
      found_missing = contains_missing(&dVa[lin],nlatanlona,has_missing_Va,
                                       missing_dVa.doubleval);
      if(found_missing) missing = missing_dVa.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < nlatbnlonb; j++) {
        dUb[lout+j] = missing;
        dVb[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
      NGCALLF(dtrvsph77,DTRVSPH77)(&intl,igrida,&nlona,&nlata,
                                   &dUa[lin],&dVa[lin],
                                   igridb,&nlonb,&nlatb,&dUb[lout],&dVb[lout],
                                   &twave,work,&lwork,wsave,&lsave,dwork,
                                   &ldwork,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: ier = %d\n", ier );
      }
    }
    lin  += nlatanlona;
    lout += nlatbnlonb;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(dwork);
  NclFree(wsave);
  if((void*)dUa != Ua) NclFree(dUa);
  if((void*)dVa != Va) NclFree(dVa);
/*
 * Return.
 */
  if(type_Ub == NCL_float) rUb = coerce_output_float(dUb,Ub,total_size_out,1);
  if(type_Vb == NCL_float) rVb = coerce_output_float(dVb,Vb,total_size_out,1);

  return(NhlNOERROR);
}

NhlErrorTypes fo2fshv_W( void )
{
/*
 * Input array vaiables
 */
  void *uoff, *voff;
  double *duoff, *dvoff;
  int ndims_uoff, ndims_voff;
  int dsizes_uoff[NCL_MAX_DIMENSIONS], dsizes_voff[NCL_MAX_DIMENSIONS];
  int jlat, jlat1, ilon, ilon2, jlatilon,jlat1ilon;
  NclScalar missing_uoff, missing_voff, missing_duoff, missing_dvoff;
  int has_missing_uoff, has_missing_voff, found_missing;
  NclBasicDataTypes type_uoff, type_voff;
  double missing;
/*
 * Output array variables
 */
  void *ureg, *vreg;
  double *dureg, *dvreg;
  float *rureg, *rvreg;
  int ndims_ureg, ndims_vreg;
  int dsizes_ureg[NCL_MAX_DIMENSIONS], dsizes_vreg[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ureg, type_vreg;
  int nlatb, nlonb;
/*
 * various
 */
  int i, j, lin, lout, ioff, ier=0;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork;
  double *work, *wsave;
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
           2);
  voff = (void*)NclGetArgValue(
           1,
           4,
           &ndims_voff, 
           dsizes_voff,
           &missing_voff,
           &has_missing_voff,
           &type_voff,
           2);
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
 * Coerce uoff and voff.
 */
  duoff = coerce_input_double(uoff,type_uoff,total_size_in,has_missing_uoff,
                              &missing_uoff,&missing_duoff,NULL);
  dvoff = coerce_input_double(voff,type_voff,total_size_in,has_missing_voff,
                              &missing_voff,&missing_dvoff,NULL);
  if(duoff == NULL || dvoff == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure ureg and vreg are double.
 */
  dureg = coerce_output_double(ureg,type_ureg,total_size_out);
  dvreg = coerce_output_double(vreg,type_vreg,total_size_out);

  if(dureg == NULL || dvreg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&duoff[lin],jlatilon,has_missing_uoff,
                                     missing_duoff.doubleval);
    if(found_missing) {
      missing = missing_duoff.doubleval;
    }
    else {
      found_missing = contains_missing(&dvoff[lin],jlatilon,has_missing_voff,
                                       missing_voff.doubleval);
      if(found_missing) missing = missing_dvoff.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < jlat1ilon; j++) {
        dureg[lout+j] = missing;
        dvreg[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
      NGCALLF(df2foshv,DF2FOSHV)(&duoff[lin],&dvoff[lin],&ilon,&jlat,
                                 &dureg[lout],&dvreg[lout],&jlat1,
                                 work,&lwork,wsave,&lsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: ier = %d\n", ier );
      }
    }
    lin  += jlatilon;
    lout += jlat1ilon;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if((void*)duoff != uoff) NclFree(duoff);
  if((void*)dvoff != voff) NclFree(dvoff);

/*
 * Return.
 */
  if(type_ureg == NCL_float) rureg = coerce_output_float(dureg,ureg,
                                                         total_size_out,1);
  if(type_vreg == NCL_float) rvreg = coerce_output_float(dvreg,vreg,
                                                         total_size_out,1);

  return(NhlNOERROR);
}


NhlErrorTypes f2foshv_W( void )
{
/*
 * Input array vaiables
 */
  void *ureg, *vreg;
  double *dureg, *dvreg;
  int ndims_ureg, ndims_vreg;
  int dsizes_ureg[NCL_MAX_DIMENSIONS], dsizes_vreg[NCL_MAX_DIMENSIONS];
  int jlat, jlat1, ilon, ilon2, jlatilon, jlat1ilon;
  NclScalar missing_ureg, missing_vreg, missing_dureg, missing_dvreg;
  int has_missing_ureg, has_missing_vreg, found_missing;
  NclBasicDataTypes type_ureg, type_vreg;
  double missing;
/*
 * Output array variables
 */
  void *uoff, *voff;
  double *duoff, *dvoff;
  float *ruoff, *rvoff;
  int ndims_uoff, ndims_voff;
  int dsizes_uoff[NCL_MAX_DIMENSIONS], dsizes_voff[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_uoff, type_voff;
  int nlatb, nlonb;
/*
 * various
 */
  int i, j, lin, lout, ioff, ier=0;
  int total_size_in, total_size_out, total_leftmost;
/*
 * Workspace variables
 */
  int lsave, lwork;
  double *work, *wsave;
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
           2);
  vreg = (void*)NclGetArgValue(
           1,
           4,
           &ndims_vreg, 
           dsizes_vreg,
           &missing_vreg,
           &has_missing_vreg,
           &type_vreg,
           2);
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
 * Coerce uoff and voff.
 */
  dureg = coerce_input_double(ureg,type_ureg,total_size_in,has_missing_ureg,
                              &missing_ureg,&missing_dureg,NULL);
  dvreg = coerce_input_double(vreg,type_vreg,total_size_in,has_missing_vreg,
                              &missing_vreg,&missing_dvreg,NULL);
  if(dureg == NULL || dvreg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Make sure uoff and voff are double.
 */
  duoff = coerce_output_double(uoff,type_uoff,total_size_out);
  dvoff = coerce_output_double(voff,type_voff,total_size_out);

  if(duoff == NULL || dvoff == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: Unable to allocate memory for coercing output arrays to double precision");
    return(NhlFATAL);
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
  lin = lout = 0;
  for(i = 1; i <= total_leftmost; i++) {
/*
 * Check for missing values in the input arrays.
 */
    found_missing = contains_missing(&dureg[lin],jlat1ilon,has_missing_ureg,
                                     missing_dureg.doubleval);
    if(found_missing) {
      missing = missing_dureg.doubleval;
    }
    else {
      found_missing = contains_missing(&dvreg[lin],jlat1ilon,has_missing_vreg,
                                       missing_vreg.doubleval);
      if(found_missing) missing = missing_dvreg.doubleval;
    }
    if(found_missing) {
      for(j = 0; j < jlatilon; j++) {
        duoff[lout+j] = missing;
        dvoff[lout+j] = missing;
      }
      NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
    }
    else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
      NGCALLF(df2foshv,DF2FOSHV)(&duoff[lout],&dvoff[lout],&ilon,&jlat,
                                 &dureg[lin],&dvreg[lin],&jlat1,
                                 work,&lwork,wsave,&lsave,&ioff,&ier);
      if (ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: ier = %d\n", ier );
      }
    }
    lout += jlatilon;
    lin  += jlat1ilon;
  }
/*
 * Free workspace arrays.
 */
  NclFree(work);
  NclFree(wsave);
  if((void*)dureg != ureg) NclFree(dureg);
  if((void*)dvreg != vreg) NclFree(dvreg);

/*
 * Return.
 */
  if(type_uoff == NCL_float) ruoff = coerce_output_float(duoff,uoff,
                                                         total_size_out,1);
  if(type_voff == NCL_float) rvoff = coerce_output_float(dvoff,voff,
                                                         total_size_out,1);
  return(NhlNOERROR);
}


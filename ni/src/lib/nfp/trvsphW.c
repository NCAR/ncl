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
#include <ncarg/gks.h>

#define min(x,y)  ((x) < (y) ? (x) : (y))
#define max(x,y)  ((x) > (y) ? (x) : (y))

NhlErrorTypes g2gshv_W( void )
{
/*
 * Input array variables
 */
  float *Ua, *Va;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va;
  int has_missing_Ua, has_missing_Va, found_missing;
  float missing;
/*
 * Output array variables
 */
  float *Ub, *Vb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, l1, total_elements, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (float*)NclGetArgValue(
           0,
           5,
           &ndims_Ua, 
           dsizes_Ua,
		   &missing_Ua,
		   &has_missing_Ua,
           NULL,
           2);
  Va = (float*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
		   &missing_Va,
		   &has_missing_Va,
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
  nlata = dsizes_Ua[ndims_Ua-2];
  nlona = dsizes_Ua[ndims_Ua-1];
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ua-2; i++) {
	total_elements *= dsizes_Ua[i];
  }
/* 
 * Get output arrays
 */
  Ub = (float*)NclGetArgValue(
           2,
           5,
           &ndims_Ub, 
           dsizes_Ub,
		   NULL,
		   NULL,
           NULL,
           1);
  Vb = (float*)NclGetArgValue(
           3,
           5,
           &ndims_Vb, 
           dsizes_Vb,
		   NULL,
		   NULL,
           NULL,
           1);
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
  nlatb = dsizes_Ub[ndims_Ub-2];
  nlonb = dsizes_Ub[ndims_Ub-1];
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
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = -2;
  igrida[1] = igridb[1] =  0;

  intl = 0;

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
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
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
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_Ua || has_missing_Va) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if( has_missing_Ua ) {
		  if( Ua[lin+j] == missing_Ua.floatval ) {
			found_missing = 1;
			missing = missing_Ua.floatval;
		  }
		}
		if( has_missing_Va ) {
		  if( Va[lin+j] == missing_Va.floatval ) {
			found_missing = 1;
			missing = missing_Va.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Ub[lout+j] = missing;
		Vb[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
	  NGCALLF(trvsph77,TRVSPH77)(&intl,igrida,&nlona,&nlata,&Ua[lin],&Va[lin],
								 igridb,&nlonb,&nlatb,&Ub[lout],&Vb[lout],
								 twave,work,&lwork,wsave,&lsave,dwork,
								 &ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gshv: ier = %d\n", ier );
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(dwork);
  free(wsave);
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
  float *Ua, *Va;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va;
  int has_missing_Ua, has_missing_Va, found_missing;
  float missing;
/*
 * Output array variables
 */
  float *Ub, *Vb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, l1, total_elements, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (float*)NclGetArgValue(
           0,
           5,
           &ndims_Ua, 
           dsizes_Ua,
		   &missing_Ua,
		   &has_missing_Ua,
           NULL,
           2);
  Va = (float*)NclGetArgValue(
           1,
           5,
           &ndims_Va, 
           dsizes_Va,
		   &missing_Va,
		   &has_missing_Va,
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
  nlata = dsizes_Ua[ndims_Ua-2];
  nlona = dsizes_Ua[ndims_Ua-1];
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ua-2; i++) {
	total_elements *= dsizes_Ua[i];
  }
/* 
 * Get output arrays
 */
  Ub = (float*)NclGetArgValue(
           2,
           5,
           &ndims_Ub, 
           dsizes_Ub,
		   NULL,
		   NULL,
           NULL,
           1);
  Vb = (float*)NclGetArgValue(
           3,
           5,
           &ndims_Vb, 
           dsizes_Vb,
		   NULL,
		   NULL,
           NULL,
           1);
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
  nlatb = dsizes_Ub[ndims_Ub-2];
  nlonb = dsizes_Ub[ndims_Ub-1];
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
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = -1;
  igridb[0] = -2;
  igrida[1] = igridb[1] =  0;

  intl = 0;

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
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
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
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_Ua || has_missing_Va) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if( has_missing_Ua ) {
		  if( Ua[lin+j] == missing_Ua.floatval ) {
			found_missing = 1;
			missing = missing_Ua.floatval;
		  }
		}
		if( has_missing_Va ) {
		  if( Va[lin+j] == missing_Va.floatval ) {
			found_missing = 1;
			missing = missing_Va.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Ub[lout+j] = missing;
		Vb[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
	  NGCALLF(trvsph77,TRVSPH77)(&intl,igrida,&nlona,&nlata,&Ua[lin],&Va[lin],
								 igridb,&nlonb,&nlatb,&Ub[lout],&Vb[lout],
								 twave,work,&lwork,wsave,&lsave,dwork,
								 &ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gshv: ier = %d\n", ier );
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(dwork);
  free(wsave);
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
  float *Ua, *Va;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va;
  int has_missing_Ua, has_missing_Va, found_missing;
  float missing;
/*
 * Output array variables
 */
  float *Ub, *Vb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
/*
 * various
 */
  int twave = 0, intl, i, j, l1, lin, lout, total_elements, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (float*)NclGetArgValue(
           0,
           4,
           &ndims_Ua, 
           dsizes_Ua,
		   &missing_Ua,
		   &has_missing_Ua,
           NULL,
           2);
  Va = (float*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
		   &missing_Va,
		   &has_missing_Va,
           NULL,
           2);
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
  nlata = dsizes_Ua[ndims_Ua-2];
  nlona = dsizes_Ua[ndims_Ua-1];
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ua-2; i++) {
	total_elements *= dsizes_Ua[i];
  }
/* 
 * Get output arrays
 */
  Ub = (float*)NclGetArgValue(
           2,
           4,
           &ndims_Ub, 
           dsizes_Ub,
		   NULL,
		   NULL,
           NULL,
           1);
  Vb = (float*)NclGetArgValue(
           3,
           4,
           &ndims_Vb, 
           dsizes_Vb,
		   NULL,
		   NULL,
           NULL,
           1);
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
  nlatb = dsizes_Ub[ndims_Ub-2];
  nlonb = dsizes_Ub[ndims_Ub-1];
/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = -2;
  igridb[0] = -1;
  igrida[1] = igridb[1] =  0;

  intl = 0;

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
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
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
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_Ua || has_missing_Va) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if( has_missing_Ua ) {
		  if( Ua[lin+j] == missing_Ua.floatval ) {
			found_missing = 1;
			missing = missing_Ua.floatval;
		  }
		}
		if( has_missing_Va ) {
		  if( Va[lin+j] == missing_Va.floatval ) {
			found_missing = 1;
			missing = missing_Va.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Ub[lout+j] = missing;
		Vb[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
	  NGCALLF(trvsph77,TRVSPH77)(&intl,igrida,&nlona,&nlata,&Ua[lin],&Va[lin],
								 igridb,&nlonb,&nlatb,&Ub[lout],&Vb[lout],
								 &twave,work,&lwork,wsave,&lsave,dwork,
								 &ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fshv: ier = %d\n", ier );
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(dwork);
  free(wsave);
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
  float *Ua, *Va;
  int ndims_Ua, ndims_Va;
  int dsizes_Ua[NCL_MAX_DIMENSIONS], dsizes_Va[NCL_MAX_DIMENSIONS];
  int nlata, nlona, igrida[2];
  NclScalar missing_Ua, missing_Va;
  int has_missing_Ua, has_missing_Va, found_missing;
  float missing;
/*
 * Output array variables
 */
  float *Ub, *Vb;
  int ndims_Ub, ndims_Vb;
  int dsizes_Ub[NCL_MAX_DIMENSIONS], dsizes_Vb[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb, igridb[2];
/*
 * various
 */
  int twave = 0, intl, i, j, l1, lin, lout, total_elements, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork, ldwork;
  int klat, klon, la1, la2, lb1, lb2, lwa, lwb;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ua = (float*)NclGetArgValue(
           0,
           4,
           &ndims_Ua, 
           dsizes_Ua,
		   &missing_Ua,
		   &has_missing_Ua,
           NULL,
           2);
  Va = (float*)NclGetArgValue(
           1,
           4,
           &ndims_Va, 
           dsizes_Va,
		   &missing_Va,
		   &has_missing_Va,
           NULL,
           2);
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
  nlata = dsizes_Ua[ndims_Ua-2];
  nlona = dsizes_Ua[ndims_Ua-1];
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ua-2; i++) {
	total_elements *= dsizes_Ua[i];
  }
/* 
 * Get output arrays
 */
  Ub = (float*)NclGetArgValue(
           2,
           4,
           &ndims_Ub, 
           dsizes_Ub,
		   NULL,
		   NULL,
           NULL,
           1);
  Vb = (float*)NclGetArgValue(
           3,
           4,
           &ndims_Vb, 
           dsizes_Vb,
		   NULL,
		   NULL,
           NULL,
           1);
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
  nlatb = dsizes_Ub[ndims_Ub-2];
  nlonb = dsizes_Ub[ndims_Ub-1];
/* 
 * igrida describes the array going in, and igridb describes the array 
 * coming out.
 */
  igrida[0] = igridb[0] = -1;
  igrida[1] = igridb[1] =  0;

  intl = 0;

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
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fshv: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each nlata x nlona grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  lin = lout = 0;
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_Ua || has_missing_Va) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if( has_missing_Ua ) {
		  if( Ua[lin+j] == missing_Ua.floatval ) {
			found_missing = 1;
			missing = missing_Ua.floatval;
		  }
		}
		if( has_missing_Va ) {
		  if( Va[lin+j] == missing_Va.floatval ) {
			found_missing = 1;
			missing = missing_Va.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Ub[lout+j] = missing;
		Vb[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'trvsph' with the full argument list.
 */
	  NGCALLF(trvsph77,TRVSPH77)(&intl,igrida,&nlona,&nlata,&Ua[lin],&Va[lin],
								 igridb,&nlonb,&nlatb,&Ub[lout],&Vb[lout],
								 &twave,work,&lwork,wsave,&lsave,dwork,
								 &ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fshv: ier = %d\n", ier );
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(dwork);
  free(wsave);
/*
 * Return output grid to NCL.
 */
  return(NhlNOERROR);
}

NhlErrorTypes fo2fshv_W( void )
{
/*
 * Input array vaiables
 */
  float *uoff, *voff;
  int ndims_uoff, ndims_voff;
  int dsizes_uoff[NCL_MAX_DIMENSIONS], dsizes_voff[NCL_MAX_DIMENSIONS];
  int jlat, jlat1, ilon;
  NclScalar missing_uoff, missing_voff;
  int has_missing_uoff, has_missing_voff, found_missing=0;
  float missing;
/*
 * Output array variables
 */
  float *ureg, *vreg;
  int ndims_ureg, ndims_vreg;
  int dsizes_ureg[NCL_MAX_DIMENSIONS], dsizes_vreg[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb;
/*
 * various
 */
  int i, j, lin, lout, total_elements, ioff, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork;
  float *work, *wsave;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  uoff = (float*)NclGetArgValue(
           0,
           4,
           &ndims_uoff, 
           dsizes_uoff,
		   &missing_uoff,
		   &has_missing_uoff,
           NULL,
           2);
  voff = (float*)NclGetArgValue(
           1,
           4,
           &ndims_voff, 
           dsizes_voff,
		   &missing_voff,
		   &has_missing_voff,
           NULL,
           2);
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
  jlat = dsizes_uoff[ndims_uoff-2];
  ilon = dsizes_uoff[ndims_uoff-1];
/* 
 * Get output arrays
 */
  ureg = (float*)NclGetArgValue(
           2,
           4,
           &ndims_ureg, 
           dsizes_ureg,
		   NULL,
		   NULL,
           NULL,
           1);
  vreg = (float*)NclGetArgValue(
           3,
           4,
           &ndims_vreg, 
           dsizes_vreg,
		   NULL,
		   NULL,
           NULL,
           1);
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_ureg != ndims_uoff || ndims_vreg != ndims_voff ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  jlat1 = jlat + 1;
  if ( (dsizes_ureg[ndims_uoff-2] != jlat1) || (dsizes_ureg[ndims_uoff-1] != ilon) ) {
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
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_uoff-2; i++) {
	total_elements *= dsizes_uoff[i];
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
  work  = (float *)calloc(lwork,sizeof(float));
  wsave = (float *)calloc(lsave,sizeof(float));
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
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_uoff || has_missing_voff) {
	  j = 0;
	  while( j < jlat*ilon && !found_missing ) {
		if( has_missing_uoff ) {
		  if( uoff[lin+j] == missing_uoff.floatval ) {
			found_missing = 1;
			missing = missing_uoff.floatval;
		  }
		}
		if( has_missing_voff ) {
		  if( voff[lin+j] == missing_voff.floatval ) {
			found_missing = 1;
			missing = missing_voff.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < jlat1*ilon; j++) {
		ureg[lout+j] = missing;
		vreg[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
	  NGCALLF(f2foshv,F2FOSHV)(&uoff[lin],&voff[lin],&ilon,&jlat,
							   &ureg[lout],&vreg[lout],&jlat1,
							   work,&lwork,wsave,&lsave,&ioff,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fshv: ier = %d\n", ier );
	  }
	}
	lin  += (jlat*ilon);
	lout += (jlat1*ilon);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(wsave);
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
  float *ureg, *vreg;
  int ndims_ureg, ndims_vreg;
  int dsizes_ureg[NCL_MAX_DIMENSIONS], dsizes_vreg[NCL_MAX_DIMENSIONS];
  int jlat, jlat1, ilon;
  NclScalar missing_ureg, missing_vreg;
  int has_missing_ureg, has_missing_vreg, found_missing;
  float missing;
/*
 * Output array variables
 */
  float *uoff, *voff;
  int ndims_uoff, ndims_voff;
  int dsizes_uoff[NCL_MAX_DIMENSIONS], dsizes_voff[NCL_MAX_DIMENSIONS];
  int nlatb, nlonb;
/*
 * various
 */
  int i, j, lin, lout, total_elements, ioff, ier=0;
/*
 * Workspace variables
 */
  int lsave, lwork;
  float *work, *wsave;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ureg = (float*)NclGetArgValue(
           0,
           4,
           &ndims_ureg, 
           dsizes_ureg,
		   &missing_ureg,
		   &has_missing_ureg,
           NULL,
           2);
  vreg = (float*)NclGetArgValue(
           1,
           4,
           &ndims_vreg, 
           dsizes_vreg,
		   &missing_vreg,
		   &has_missing_vreg,
           NULL,
           2);
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
  jlat1 = dsizes_ureg[ndims_ureg-2];
  ilon  = dsizes_ureg[ndims_ureg-1];
/* 
 * Get output arrays
 */
  uoff = (float*)NclGetArgValue(
           2,
           4,
           &ndims_uoff, 
           dsizes_uoff,
		   NULL,
		   NULL,
           NULL,
           1);
  voff = (float*)NclGetArgValue(
           3,
           4,
           &ndims_voff, 
           dsizes_voff,
		   NULL,
		   NULL,
           NULL,
           1);
/*
 * The grids going out must be at least 2-dimensional and have the same
 * size. The input and output grids must have the same number of dimensions
 * and all but the last two dimensions must be the same.
 */
  if( ndims_uoff != ndims_ureg || ndims_voff != ndims_vreg ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2foshv: The output arrays must have the same number of dimensions as the input arrays");
    return(NhlFATAL);
  }
  jlat = jlat1 - 1;
  if ( (dsizes_uoff[ndims_ureg-2] != jlat) || (dsizes_uoff[ndims_ureg-1] != ilon) ) {
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
/*
 * Compute the total number of elements in the input arrays.
 */
  total_elements = 1;
  for(i = 0; i < ndims_ureg-2; i++) {
	total_elements *= dsizes_ureg[i];
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
  work  = (float *)calloc(lwork,sizeof(float));
  wsave = (float *)calloc(lsave,sizeof(float));
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
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values in the input arrays.
 */
	found_missing = 0;
	if(has_missing_ureg || has_missing_vreg) {
	  j = 0;
	  while( j < jlat1*ilon && !found_missing ) {
		if( has_missing_ureg ) {
		  if( ureg[lin+j] == missing_ureg.floatval ) {
			found_missing = 1;
			missing = missing_ureg.floatval;
		  }
		}
		if( has_missing_vreg ) {
		  if( vreg[lin+j] == missing_vreg.floatval ) {
			found_missing = 1;
			missing = missing_vreg.floatval;
		  }
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < jlat*ilon; j++) {
		uoff[lout+j] = missing;
		voff[lout+j] = missing;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: One or more 2-dimensional input arrays contain missing values. No interpolation performed on these 2d arrays.");
	}
	else {
/*
 * Call the f77 version of 'f2foshv' with the full argument list.
 */
	  NGCALLF(f2foshv,F2FOSHV)(&uoff[lin],&voff[lin],&ilon,&jlat,
							   &ureg[lout],&vreg[lout],&jlat1,
							   work,&lwork,wsave,&lsave,&ioff,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2foshv: ier = %d\n", ier );
	  }
	}
	lin  += (jlat*ilon);
	lout += (jlat1*ilon);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(wsave);
/*
 * Return.
 */
  return(NhlNOERROR);
}



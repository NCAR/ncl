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

NhlErrorTypes g2gsh_W( void )
{
/*
 * Input array variables
 */
  float *Ta;
  int ndims_Ta, dsizes_Ta[NCL_MAX_DIMENSIONS], nlata, nlona, igrida[2];
  NclScalar missing_Ta;
  int has_missing_Ta, found_missing;
/*
 * Output array variables
 */
  float *Tb;
  int *dsizes_Tb, dsizes_Tb2[NCL_MAX_DIMENSIONS], nlatb, nlonb, igridb[2];
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, total_elements;
/*
 * Workspace variables
 */
  int lsave, lsvmin, lwork, ldwork, lwkmin, ker = 0;
  int klat, klon, k1, k2, lwa, lwb, ier = 0;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (float*)NclGetArgValue(
           0,
           3,
           &ndims_Ta, 
           dsizes_Ta,
		   &missing_Ta,
		   &has_missing_Ta,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  nlata = dsizes_Ta[ndims_Ta-2];
  nlona = dsizes_Ta[ndims_Ta-1];
/*
 * Compute the total number of elements in our array.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ta-2; i++) {
	total_elements *= dsizes_Ta[i];
  }
/*
 * Get sizes for output array, and check to be sure our lat/lon dimensions
 * have at least four elements.
 */
  dsizes_Tb = (int*)NclGetArgValue(
                1,
                3,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                2);
  nlatb = dsizes_Tb[0];
  nlonb = dsizes_Tb[1];
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
  Tb = (float *)calloc((nlatb*nlonb)*total_elements,sizeof(float));
  if( Tb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
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
 * Dynamically allocate the various work space.
 */
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: workspace allocate failed\n" );
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
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_Ta) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if(Ta[lin+j] == missing_Ta.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Tb[lout+j] = missing_Ta.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gsh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
	  NGCALLF(trssph,TRSSPH)(&intl,igrida,&nlona,&nlata,&Ta[lin],
							 igridb,&nlonb,&nlatb,&Tb[lout],
							 wsave,&lsave,&lsvmin,work,&lwork,&lwkmin,
							 dwork,&ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"g2gsh: ier = %d\n", ier );
	  }

	  if (abs(*twave)) {
		free(dwork);
		ldwork = 2*klat*(klat+1)+1;
		dwork  = (double *)calloc(ldwork,sizeof(double));
		if (dwork == NULL) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"g2gsh: workspace allocate failed\n" );
		  return(NhlFATAL);
		}
/*
 * Truncate the data at a specified truncation.
 */
		NGCALLF(trcwav,TRCWAV)(igridb,&nlatb,&nlonb,&Tb[lout],
							   wsave,&lsave,work,&lwork,dwork,&ldwork,
							   &ker,twave);
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) free(work);
  if(dwork != NULL) free(dwork);
  if(wsave != NULL) free(wsave);
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
  if(has_missing_Ta) {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,&missing_Ta,
						  NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,NULL,NCL_float,0));
  }
}


NhlErrorTypes f2gsh_W( void )
{
/*
 * Input array variables
 */
  float *Ta;
  int ndims_Ta, dsizes_Ta[NCL_MAX_DIMENSIONS], nlata, nlona, igrida[2];
  NclScalar missing_Ta;
  int has_missing_Ta, found_missing;
/*
 * Output array variables
 */
  float *Tb;
  int *dsizes_Tb, dsizes_Tb2[NCL_MAX_DIMENSIONS], nlatb, nlonb, igridb[2];
/*
 * various
 */
  int *twave, intl, i, j, lin, lout, total_elements;
/*
 * Workspace variables
 */
  int lsave, lsvmin, lwork, ldwork, lwkmin, ker = 0;
  int klat, klon, k1, k2, lwa, lwb, ier = 0;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (float*)NclGetArgValue(
           0,
           3,
           &ndims_Ta, 
           dsizes_Ta,
		   &missing_Ta,
		   &has_missing_Ta,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  nlata = dsizes_Ta[ndims_Ta-2];
  nlona = dsizes_Ta[ndims_Ta-1];
/*
 * Compute the total number of elements in our array.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ta-2; i++) {
	total_elements *= dsizes_Ta[i];
  }
/*
 * Get sizes for output array, and check to be sure our lat/lon dimensions
 * have at least four elements.
 */
  dsizes_Tb = (int*)NclGetArgValue(
                1,
                3,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                2);
  nlatb = dsizes_Tb[0];
  nlonb = dsizes_Tb[1];
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
  Tb = (float *)calloc((nlatb*nlonb)*total_elements,sizeof(float));
  if( Tb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
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
 * Dynamically allocate the various work space.
 */
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: workspace allocate failed\n" );
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
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_Ta) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if(Ta[lin+j] == missing_Ta.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Tb[lout+j] = missing_Ta.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gsh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
	  NGCALLF(trssph,TRSSPH)(&intl,igrida,&nlona,&nlata,&Ta[lin],
							 igridb,&nlonb,&nlatb,&Tb[lout],
							 wsave,&lsave,&lsvmin,work,&lwork,&lwkmin,
							 dwork,&ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2gsh: ier = %d\n", ier );
	  }

	  if (abs(*twave)) {
		free(dwork);
		ldwork = 2*klat*(klat+1)+1;
		dwork  = (double *)calloc(ldwork,sizeof(double));
		if (dwork == NULL) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"f2gsh: workspace allocate failed\n" );
		  return(NhlFATAL);
		}
/*
 * Truncate the data at a specified truncation.
 */
		NGCALLF(trcwav,TRCWAV)(igridb,&nlatb,&nlonb,&Tb[lout],
							   wsave,&lsave,work,&lwork,dwork,&ldwork,
							   &ker,twave);
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) free(work);
  if(dwork != NULL) free(dwork);
  if(wsave != NULL) free(wsave);
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
  if(has_missing_Ta) {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,&missing_Ta,
						  NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,NULL,NCL_float,0));
  }
}


NhlErrorTypes g2fsh_W( void )
{
/*
 * Input array variables
 */
  float *Ta;
  int ndims_Ta, dsizes_Ta[NCL_MAX_DIMENSIONS], nlata, nlona, igrida[2];
  NclScalar missing_Ta;
  int has_missing_Ta, found_missing;
/*
 * Output array variables
 */
  float *Tb;
  int *dsizes_Tb, dsizes_Tb2[NCL_MAX_DIMENSIONS], nlatb, nlonb, igridb[2];
/*
 * various
 */
  int intl, i, j, lin, lout, total_elements;
/*
 * Workspace variables
 */
  int lsave, lsvmin, lwork, ldwork, lwkmin, ker = 0;
  int klat, klon, k1, k2, lwa, lwb, ier = 0;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (float*)NclGetArgValue(
           0,
           2,
           &ndims_Ta, 
           dsizes_Ta,
		   &missing_Ta,
		   &has_missing_Ta,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  nlata = dsizes_Ta[ndims_Ta-2];
  nlona = dsizes_Ta[ndims_Ta-1];
/*
 * Compute the total number of elements in our array.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ta-2; i++) {
	total_elements *= dsizes_Ta[i];
  }
/*
 * Get sizes for output array, and check to be sure our lat/lon dimensions
 * have at least four elements.
 */
  dsizes_Tb = (int*)NclGetArgValue(
                1,
                2,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                2);
  nlatb = dsizes_Tb[0];
  nlonb = dsizes_Tb[1];
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
  Tb = (float *)calloc((nlatb*nlonb)*total_elements,sizeof(float));
  if( Tb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
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
 * Dynamically allocate the various work space.
 */
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL || dwork == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"g2fsh: workspace allocate failed\n" );
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
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_Ta) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if(Ta[lin+j] == missing_Ta.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Tb[lout+j] = missing_Ta.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fsh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
	  NGCALLF(trssph,TRSSPH)(&intl,igrida,&nlona,&nlata,&Ta[lin],
							 igridb,&nlonb,&nlatb,&Tb[lout],
							 wsave,&lsave,&lsvmin,work,&lwork,&lwkmin,
							 dwork,&ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"g2fsh: ier = %d\n", ier );
	  }

	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }

/*
 * Free workspace arrays.
 */
  if (work != NULL) free(work);
  if(dwork != NULL) free(dwork);
  if(wsave != NULL) free(wsave);
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
  if(has_missing_Ta) {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,&missing_Ta,
						  NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,NULL,NCL_float,0));
  }
}



NhlErrorTypes f2fsh_W( void )
{
/*
 * Input array variables
 */
  float *Ta;
  int ndims_Ta, dsizes_Ta[NCL_MAX_DIMENSIONS], nlata, nlona, igrida[2];
  NclScalar missing_Ta;
  int has_missing_Ta, found_missing;
/*
 * Output array variables
 */
  float *Tb;
  int *dsizes_Tb, dsizes_Tb2[NCL_MAX_DIMENSIONS], nlatb, nlonb, igridb[2];
/*
 * various
 */
  int intl, i, j, lin, lout, total_elements;
/*
 * Workspace variables
 */
  int lsave, lsvmin, lwork, ldwork, lwkmin, ker = 0;
  int klat, klon, k1, k2, lwa, lwb, ier = 0;
  float *work, *wsave;
  double *dwork;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  Ta = (float*)NclGetArgValue(
           0,
           2,
           &ndims_Ta, 
           dsizes_Ta,
		   &missing_Ta,
		   &has_missing_Ta,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_Ta < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  nlata = dsizes_Ta[ndims_Ta-2];
  nlona = dsizes_Ta[ndims_Ta-1];
/*
 * Compute the total number of elements in our array.
 */
  total_elements = 1;
  for(i = 0; i < ndims_Ta-2; i++) {
	total_elements *= dsizes_Ta[i];
  }
/*
 * Get sizes for output array, and check to be sure our lat/lon dimensions
 * have at least four elements.
 */
  dsizes_Tb = (int*)NclGetArgValue(
                1,
                2,
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
                2);
  nlatb = dsizes_Tb[0];
  nlonb = dsizes_Tb[1];
  if( nlatb < 4 || nlonb < 4 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: The lat/lon dimensions of the output array must be at least 4");
    return(NhlFATAL);
  }
  Tb = (float *)calloc((nlatb*nlonb)*total_elements,sizeof(float));
  if( Tb == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
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
 * Dynamically allocate the various work space.
 */
  work  = (float *)calloc(lwork,sizeof(float));
  dwork  = (double *)calloc(ldwork,sizeof(double));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL || dwork == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fsh: workspace allocate failed\n" );
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
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_Ta) {
	  j = 0;
	  while( j < nlata*nlona && !found_missing ) {
		if(Ta[lin+j] == missing_Ta.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < nlatb*nlonb; j++) {
		Tb[lout+j] = missing_Ta.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fsh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'trssph' with the full argument list.
 */
	  NGCALLF(trssph,TRSSPH)(&intl,igrida,&nlona,&nlata,&Ta[lin],
							 igridb,&nlonb,&nlatb,&Tb[lout],
							 wsave,&lsave,&lsvmin,work,&lwork,&lwkmin,
							 dwork,&ldwork,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fsh: ier = %d\n", ier );
	  }
	}
	lin  += (nlata*nlona);
	lout += (nlatb*nlonb);
  }
/*
 * Free workspace arrays.
 */
  if (work != NULL) free(work);
  if(dwork != NULL) free(dwork);
  if(wsave != NULL) free(wsave);
/*
 * Return output grid to NCL.
 */
  for( i = 0; i < ndims_Ta-2; i++ ) dsizes_Tb2[i] = dsizes_Ta[i];
  dsizes_Tb2[ndims_Ta-2] = nlatb;
  dsizes_Tb2[ndims_Ta-1] = nlonb;
  if(has_missing_Ta) {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,&missing_Ta,
						  NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)Tb,ndims_Ta,dsizes_Tb2,NULL,NCL_float,0));
  }
}


NhlErrorTypes fo2fsh_W( void )
{
/*
 * Input array variables
 */
  float *goff;
  int ndims_goff, jlat, jlat1, ilon;
  int dsizes_goff[NCL_MAX_DIMENSIONS];
  NclScalar missing_goff;
  int has_missing_goff, found_missing;
/*
 * Output array variables
 */
  float *greg;
  int *dsizes_greg;
/*
 * Workspace variables
 */
  int lwork, lsave;
  float *work, *wsave;
/*
 * error code, various
 */
  int i, j, lin, lout;
  int total_elements, ioff, ier = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  goff = (float*)NclGetArgValue(
           0,
           1,
           &ndims_goff, 
           dsizes_goff,
		   &missing_goff,
		   &has_missing_goff,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_goff < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  jlat = dsizes_goff[ndims_goff-2];
  ilon = dsizes_goff[ndims_goff-1];
  jlat1 = jlat + 1;
/*
 * Compute number of elements.
 */
  total_elements = 1;
  for(i = 0; i < ndims_goff-2; i++) {
	total_elements *= dsizes_goff[i];
  }
/*
 * Get sizes for output array.
 */
  dsizes_greg = (int *)calloc(ndims_goff,sizeof(int));
  if( dsizes_greg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: Unable to allocate memory for output dimension array");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_goff-2; i++ ) dsizes_greg[i] = dsizes_goff[i];
  dsizes_greg[ndims_goff-2] = jlat1;
  dsizes_greg[ndims_goff-1] = ilon;
  greg = (float *)calloc(total_elements*jlat1*ilon,sizeof(float));
  if( greg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: Unable to allocate memory for output array");
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
  work  = (float *)calloc(lwork,sizeof(float));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fo2fsh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  lin = lout = 0;
  ioff = 0;
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_goff) {
	  j = 0;
	  while( j < jlat*ilon && !found_missing ) {
		if(goff[lin+j] == missing_goff.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < jlat1*ilon; j++) {
		greg[lout+j] = missing_goff.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fsh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'fo2fsh' with the full argument list.
 */
	  NGCALLF(fo2f,FO2F)(&goff[lin],&ilon,&jlat,&greg[lout],&jlat1,
						 work,&lwork,wsave,&lsave,&ioff,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"fo2fsh: ier = %d\n", ier );
	  }
	}
	lin  += (ilon*jlat);
	lout += (ilon*jlat1);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(wsave);
/*
 * Return output grid to NCL.
 */
  if(has_missing_goff) {
	return(NclReturnValue((void*)greg,ndims_goff,dsizes_greg,&missing_goff,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)greg,ndims_goff,dsizes_greg,NULL,NCL_float,0));
  }
}


NhlErrorTypes f2fosh_W( void )
{
/*
 * Input array variables
 */
  float *greg;
  int ndims_greg, jlat, jlat1, ilon;
  int dsizes_greg[NCL_MAX_DIMENSIONS];
  NclScalar missing_greg;
  int has_missing_greg, found_missing;
/*
 * Output array variables
 */
  float *goff;
  int *dsizes_goff;
/*
 * Workspace variables
 */
  int lwork, lsave;
  float *work, *wsave;
/*
 * error code, various
 */
  int i, j, lin, lout;
  int total_elements, ioff, ier = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  greg = (float*)NclGetArgValue(
           0,
           1,
           &ndims_greg, 
           dsizes_greg,
		   &missing_greg,
		   &has_missing_greg,
           NULL,
           2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_greg < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: The input array must be at least a 2-dimensional array");
    return(NhlFATAL);
  }
  jlat1 = dsizes_greg[ndims_greg-2];
  ilon  = dsizes_greg[ndims_greg-1];
/*
 * Compute number of elements.
 */
  total_elements = 1;
  for(i = 0; i < ndims_greg-2; i++) {
	total_elements *= dsizes_greg[i];
  }
/*
 * Get sizes for output array.
 */
  dsizes_goff = (int *)calloc(ndims_greg,sizeof(int));
  if( dsizes_goff == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: Unable to allocate memory for output dimension array");
    return(NhlFATAL);
  }
  jlat = jlat1 - 1;
  for( i = 0; i < ndims_greg-2; i++ ) dsizes_goff[i] = dsizes_greg[i];
  dsizes_goff[ndims_greg-2] = jlat;
  dsizes_goff[ndims_greg-1] = ilon;
  goff = (float *)calloc(total_elements*jlat*ilon,sizeof(float));
  if( goff == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: Unable to allocate memory for output array");
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
  work  = (float *)calloc(lwork,sizeof(float));
  wsave = (float *)calloc(lsave,sizeof(float));
  if (work == NULL || wsave == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"f2fosh: workspace allocate failed\n" );
    return(NhlFATAL);
  }
/*
 * Loop through each jlat x ilon grid.  If it contains any missing values,
 * then return all missing values for that grid.  Otherwise, keep going 
 * and do the regridding.
 */ 
  lin = lout = 0;
  ioff = 1;
  for(i = 1; i <= total_elements; i++) {
/*
 * Check for missing values.
 */
	found_missing = 0;
	if(has_missing_greg) {
	  j = 0;
	  while( j < jlat1*ilon && !found_missing ) {
		if(greg[lin+j] == missing_greg.floatval) {
		  found_missing = 1;
		}
		j++;
	  }
	}
	if(found_missing) {
	  for(j = 0; j < jlat*ilon; j++) {
		goff[lout+j] = missing_greg.floatval;
	  }
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fosh: A 2-dimensional input array contains missing values. No interpolation performed on this 2d array.");
	}
	else {
/*
 * Call the f77 version of 'f2fosh' with the full argument list.
 */
	  NGCALLF(fo2f,FO2F)(&goff[lin],&ilon,&jlat,&greg[lout],&jlat1,
						 work,&lwork,wsave,&lsave,&ioff,&ier);
	  if (ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"f2fosh: ier = %d\n", ier );
	  }
	}
	lin  += (ilon*jlat1);
	lout += (ilon*jlat);
  }
/*
 * Free workspace arrays.
 */
  free(work);
  free(wsave);
/*
 * Return output grid to NCL.
 */
  if(has_missing_greg) {
	return(NclReturnValue((void*)goff,ndims_greg,dsizes_goff,&missing_greg,NCL_float,0));
  }
  else {
	return(NclReturnValue((void*)goff,ndims_greg,dsizes_goff,NULL,NCL_float,0));
  }
}



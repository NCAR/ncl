#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(dfourinfo,DFOURINFO)(double*,int*,int*,double*,double*,
                                         double*);

NhlErrorTypes fourier_info_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *tmp_x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int found_missing, found_any_missing;
  NclScalar missing_x, missing_dx, missing_rx;
  NclBasicDataTypes type_x;
  int *nhret, nht;
/*
 * Output array variables
 */
  void *finfo;
  double *tmp_amp, *tmp_pha, *tmp_pcv;
  NclBasicDataTypes type_finfo;
  int ndims_finfo, *dsizes_finfo;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_x, index_amp, index_pha, index_pcv;
  int npts, size_leftmost, size_output;
/*
 * Retrieve arguments.
 */
  x = (void*)NclGetArgValue(
          0,
          2,
          &ndims_x,
          dsizes_x,
          &missing_x,
          &has_missing_x,
          &type_x,
          2);

  nhret = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check input sizes.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fourier_info: The last dimension of x must be greater than 1");
    return(NhlFATAL);
  }
  
  if(*nhret == 0) {
    nht = npts/2;
  }
  else {
    if(*nhret < 0 || *nhret > npts/2) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fourier_info: nhx must be 0 <= nhx <= npts/2, where npts is the last dimension size of x");
      return(NhlFATAL);
    }
    nht = *nhret;
  }
/*
 * Compute the total size of the output array.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_x-1; i++ ) size_leftmost *= dsizes_x[i];

/*
 * Check for missing values.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce data to double no matter what, since input array also becomes
 * output array. 
 */
  if(type_x != NCL_double) {
    tmp_x = (double*)calloc(npts,sizeof(double));
    if( tmp_x == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fourier_info: Unable to allocate memory for coercing input array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output array and other output variables. 
 */
  size_output = nht*3*size_leftmost;
  ndims_finfo = ndims_x + 1;
  dsizes_finfo = (int *)calloc(ndims_finfo,sizeof(int));
  dsizes_finfo[0] = 3;
  for( i = 1; i <= ndims_x-1; i++ ) {
    dsizes_finfo[i] = dsizes_x[i-1];
  }
  dsizes_finfo[ndims_x] = nht;

  if(type_x != NCL_double) {
    type_finfo = NCL_float;
    finfo = (void *)calloc(size_output,sizeof(float));
  }
  else {
    type_finfo = NCL_double;
    finfo = (void *)calloc(size_output,sizeof(double));
  }
  if(finfo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fourier_info: Cannot allocate space for ouput array");
    return(NhlFATAL);
  }
  if(type_finfo != NCL_double) {
    tmp_amp = (double *)calloc(nht,sizeof(double));
    tmp_pha = (double *)calloc(nht,sizeof(double));
    tmp_pcv = (double *)calloc(nht,sizeof(double));
    if(tmp_amp == NULL || tmp_pha == NULL || tmp_pcv == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"fourier_info: Cannot allocate space for temporary ouput arrays");
      return(NhlFATAL);
    }
  }
/*
 * Call the Fortran version of this routine.
 */
  found_any_missing = 0;
  index_x   = index_amp = 0;
  index_pha = nht * size_leftmost;
  index_pcv = nht * size_leftmost * 2;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_x != NCL_double) {
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double(x,tmp_x,index_x,type_x,npts,0,NULL,NULL);
    }
    else {
/*
 * Point temporary arrays to appropriate locations.
 */
      tmp_x   = &((double*)x)[index_x];
      tmp_amp = &((double*)finfo)[index_amp];
      tmp_pha = &((double*)finfo)[index_pha];
      tmp_pcv = &((double*)finfo)[index_pcv];
    }
/*
 * Check for missing values.
 */
    found_missing = contains_missing(tmp_x,npts,has_missing_x,
                                     missing_dx.doubleval);
    if(found_missing) {
      found_any_missing = 1;
      set_subset_output_missing(finfo,index_amp,type_finfo,nht,
                                missing_dx.doubleval);
      set_subset_output_missing(finfo,index_pha,type_finfo,nht,
                                missing_dx.doubleval);
      set_subset_output_missing(finfo,index_pcv,type_finfo,nht,
                                missing_dx.doubleval);
      NhlPError(NhlWARNING,NhlEUNKNOWN,"fourier_info: An input array contains missing values. No analysis performed on this array.");
    }
    else {

      NGCALLF(dfourinfo,DFOURINFO)(tmp_x,&npts,&nht,tmp_amp,tmp_pha,tmp_pcv);

      if(type_finfo != NCL_double) {
        for(j = 0; j < nht; j++) {
          ((float*)finfo)[index_amp+j] = (float)(tmp_amp[j]);
          ((float*)finfo)[index_pha+j] = (float)(tmp_pha[j]);
          ((float*)finfo)[index_pcv+j] = (float)(tmp_pcv[j]);
        }
      }
    }
    index_x   += npts;
    index_amp += nht;
    index_pha += nht;
    index_pcv += nht;
  }
/*
 * Free memory.
 */
  if(type_x != NCL_double) {
    NclFree(tmp_x);
    NclFree(tmp_amp);
    NclFree(tmp_pha);
    NclFree(tmp_pcv);
  }

/*
 * Get ready to return all this stuff to NCL.
 */
  if(found_any_missing) {
    if(type_x != NCL_double) {
/*
 * Return float values with missing value set.
 */
      return(NclReturnValue(finfo,ndims_finfo,dsizes_finfo,&missing_rx,
                            type_finfo,0));
    }
    else {
      return(NclReturnValue(finfo,ndims_finfo,dsizes_finfo,&missing_dx,
                            type_finfo,0));
    }
  }
  else {
/*
 * Return values with no missing value set.
 */
    return(NclReturnValue(finfo,ndims_finfo,dsizes_finfo,NULL,type_finfo,0));
  }
}

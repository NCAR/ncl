#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include "wrapper.h"

extern void NGCALLF(dint2p,DINT2P)(double *,double *,double *,double *,
                                   int *,double *,double *,int *,int *,
                                   double *,int*);

NhlErrorTypes int2p_W( void )
{
/*
 * Input array variables
 */
  void *pin, *xin, *pout;
  double *tmp_pin, *tmp_xin, *tmp_pout;
  int ndims_pin, dsizes_pin[NCL_MAX_DIMENSIONS];
  NclScalar missing_pin, missing_xin, missing_dx, missing_rx;
  int has_missing_pin, has_missing_xin;
  int ndims_xin, dsizes_xin[NCL_MAX_DIMENSIONS];
  int ndims_pout, dsizes_pout[NCL_MAX_DIMENSIONS];
  int *linlog;
  NclBasicDataTypes type_pin, type_xin, type_pout;
/*
 * work arrays
 */
  double *p, *x;
/*
 * output variable 
 */
  void *xout;
  double *tmp_xout;
  int size_pout, size_leftmost;
  NclBasicDataTypes type_xout;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_in, index_out, npin, npout, ier = 0, nmiss = 0, nmono = 0;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */

/*
 * Retrieve argument #1
 */
  pin = (void*)NclGetArgValue(
          0,
          4,
          &ndims_pin,
          dsizes_pin,
          &missing_pin,
          &has_missing_pin,
          &type_pin,
          2);
/*
 * Retrieve argument #2
 */
  xin = (void*)NclGetArgValue(
          1,
          4,
          &ndims_xin,
          dsizes_xin,
          &missing_xin,
          &has_missing_xin,
          &type_xin,
          2);
/*
 * Retrieve argument #3
 */
  pout = (void*)NclGetArgValue(
          2,
          4,
          &ndims_pout,
          dsizes_pout,
          NULL,
          NULL,
          &type_pout,
          2);

/*
 * Retrieve argument #4
 */
  linlog = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check number of dimensions and/or dimension sizes for arguments pin,
 * xin, and pout.
 */
  if (ndims_pin != ndims_xin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same number of dimensions");
    return(NhlFATAL);
  }
  if (ndims_pout != ndims_pin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: 'pout' must have the same number of dimensions as 'pin'");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_pin; i++ ) {
    if (dsizes_pin[i] != dsizes_xin[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The two input arrays must have the same dimensions");
      return(NhlFATAL);
    }
  }

  size_leftmost = 1;
  for( i = 0; i < ndims_pout-1; i++ ) {
    if (dsizes_pout[i] != dsizes_pin[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The 'pout' array must have the same leftmost dimensions as the 'pin' array");
      return(NhlFATAL);
    }
    size_leftmost *= dsizes_pout[i];
  }
  npin  = dsizes_pin[ndims_pin-1];
  npout = dsizes_pout[ndims_pout-1];
  if (npin < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The right-most dimension of 'pin' must be at least two");
    return(NhlFATAL);
  }

  size_pout = size_leftmost * npout;

/*
 * Check for missing values.
 */
  if(has_missing_xin) {
    coerce_missing(type_xin,has_missing_xin,&missing_xin,&missing_dx,
                   &missing_rx);
  }
  else if(has_missing_pin) {
    coerce_missing(type_pin,has_missing_pin,&missing_pin,&missing_dx,
                   &missing_rx);
  }
  else {
/*
 * Assign a default missing value.
 */ 
    if(type_pin != NCL_double && type_xin != NCL_double && 
       type_pout != NCL_double) {
      missing_rx.floatval  = 1.e36;
    }
    missing_dx.doubleval = 1.e36;
  }
/*
 * Coerce data to double if necessary.
 */
  if(type_xin != NCL_double) {
    tmp_xin = (double*)calloc(npin,sizeof(double));
    if( tmp_xin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing xin array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_pin != NCL_double) {
    tmp_pin = (double*)calloc(npin,sizeof(double));
    if( tmp_pin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pin array to double precision");
      return(NhlFATAL);
    }
  }
  if(type_pout != NCL_double) {
    tmp_pout = (double*)calloc(npout,sizeof(double));
    if( tmp_pout == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pout array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output.
 */
  if(type_xin  != NCL_double && type_pin != NCL_double &&
     type_pout != NCL_double) {

    type_xout = NCL_float;

    xout     = (void*)calloc(size_pout,sizeof(float));
    tmp_xout = (double*)calloc(npout,sizeof(double));
    if(xout == NULL || tmp_xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_xout = NCL_double;
    xout = (void*)calloc(size_pout,sizeof(double));
    if( xout == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for work arrays.
 */
  p = (double*)calloc(npin,sizeof(double));
  x = (double*)calloc(npin,sizeof(double));
  if (p == NULL || x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate space for work arrays\n" );
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  index_in = index_out = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    if(type_pin != NCL_double) {
/*
 * Coerce npin subsection of pin (tmp_pin) to double.
 */
      coerce_subset_input_double(pin,tmp_pin,index_in,type_pin,npin,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_pin to appropriate location in pin.
 */
      tmp_pin = &((double*)pin)[index_in];
    }
    if(type_xin != NCL_double) {
/*
 * Coerce npin subsection of xin (tmp_xin) to double.
 */
      coerce_subset_input_double(xin,tmp_xin,index_in,type_xin,npin,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_xin to appropriate location in xin.
 */
      tmp_xin = &((double*)xin)[index_in];
    }
    if(type_pout != NCL_double) {
/*
 * Coerce npout subsection of pout (tmp_pout) to double.
 */
      coerce_subset_input_double(pout,tmp_pout,index_out,type_pout,npout,
                                 0,NULL,NULL);
    }
    else {
/*
 * Point tmp_pout to appropriate location in pout.
 */
      tmp_pout = &((double*)pout)[index_out];
    }

    if(type_xout == NCL_double) tmp_xout = &((double*)xout)[index_out];


    NGCALLF(dint2p,DINT2P)(tmp_pin,tmp_xin,&p[0],&x[0],&npin,
                           tmp_pout,tmp_xout,&npout,linlog,
                           &missing_dx.doubleval,&ier);
    if (ier) {
      if (ier >= 1000) nmiss++;
      else             nmono++;
      set_subset_output_missing(xout,index_out,type_xout,npout,
				missing_dx.doubleval);
    }
    else {
/*
 * Copy output values from temporary tmp_xout to xout.
 */
      if(type_xout != NCL_double) {
	for(j = 0; j < npout; j++) {
	  ((float*)xout)[index_out+j] = (float)(tmp_xout[j]);
	}
      }
    }
    index_in  += npin;
    index_out += npout;
  }
  if (nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"int2p: %d input array(s) contained all missing data. No interpolation performed on these arrays",nmiss);
  }
  if (nmono) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"int2p: %d pin array(s) were in a different order than the corresponding pout array(s). No interpolation performed on these arrays",nmono);
  }
/*
 * Free memory.
 */
  NclFree(p);
  NclFree(x);
  if(type_xin  != NCL_double) NclFree(tmp_xin);
  if(type_pin  != NCL_double) NclFree(tmp_pin);
  if(type_pout != NCL_double) NclFree(tmp_pout);
  if(type_xout != NCL_double) NclFree(tmp_xout);

/*
 * Return values.
 */
  if(type_xout != NCL_double) {
/*
 * Return float values with missing value set.
 */
    return(NclReturnValue(xout,ndims_pout,dsizes_pout,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values with missing value set.
 */
    return(NclReturnValue(xout,ndims_pout,dsizes_pout,&missing_dx,
                          NCL_double,0));
  }
}

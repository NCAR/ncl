#include <stdio.h>
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
  int size_xout, size_leftmost, *dsizes_xout;
  NclBasicDataTypes type_xout;
/*
 * Declare various variables for random purposes.
 */
  int i, j, index_in, index_out, npin, npout, ier = 0, ret;
  int nmiss = 0, nmono = 0;
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
          DONT_CARE);
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
          DONT_CARE);
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
          DONT_CARE);

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
          DONT_CARE);

/*
 * Check dimension sizes for pin and xin. If any of them are multiple
 * dimensions, then all but the last (rightmost) dimension must be the same.
 */
  npin = dsizes_pin[ndims_pin-1];

  if (npin < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: The rightmost dimension of pin must be at least two");
    return(NhlFATAL);
  }

  if(ndims_pin != ndims_xin && ndims_pin != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: pin must either be a 1-dimensional array or an array the same size as xin");
    return(NhlFATAL);
  }

  if(ndims_pin == ndims_xin) {
    for( i = 0; i < ndims_pin; i++ ) {
      if (dsizes_pin[i] != dsizes_xin[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: If xin and pin have the same number of dimensions, then they must be the same dimension sizes");
        return(NhlFATAL);
      }
    }
  }
  else {
    if (dsizes_xin[ndims_xin-1] != npin) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: If pin is a one-dimensional array, then it must be the same size as the righmost dimension of xin");
      return(NhlFATAL);
    }
  }

/*
 * Check dimension sizes for pout. If it is multi-dimensional, then
 * it must have the same rightmost dimensions as xin.
 */
  npout = dsizes_pout[ndims_pout-1];

  if(ndims_pout != ndims_xin && ndims_pout != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: pout must either be a 1-dimensional array or an array with the same number of dimensions as xin");
    return(NhlFATAL);
  }

  if(ndims_pout > 1) {
    for( i = 0; i < ndims_pout-1; i++ ) {
      if (dsizes_pout[i] != dsizes_xin[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: If xin and pout have the same number of dimensions, then all but their last dimension must be the same size");
        return(NhlFATAL);
      }
    }
  }
/*
 * Calculate the size of the leftmost dimensions of xin (if any).
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_xin-1; i++ ) size_leftmost *= dsizes_xin[i];

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
 * Create temporary arrays to coerce input to double, if necessary.
 */
  if(type_xin != NCL_double) {
    tmp_xin = (double*)calloc(npin,sizeof(double));
    if( tmp_xin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing xin array to double precision");
      return(NhlFATAL);
    }
  }
  if(ndims_pin == 1) {
    tmp_pin = coerce_input_double(pin,type_pin,npin,0,NULL,NULL);
  }
  else {
    if(type_pin != NCL_double) {
      tmp_pin = (double*)calloc(npin,sizeof(double));
      if( tmp_pin == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pin array to double precision");
        return(NhlFATAL);
      }
    }
  }
  if(ndims_pout == 1) {
    tmp_pout = coerce_input_double(pout,type_pout,npout,0,NULL,NULL);
  }
  else {
    if(type_pout != NCL_double) {
      tmp_pout = (double*)calloc(npout,sizeof(double));
      if( tmp_pout == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for coercing pout array to double precision");
        return(NhlFATAL);
      }
    }
  }
/*
 * Allocate space for output.
 */
  dsizes_xout = (int*)calloc(ndims_xin,sizeof(int));
  if(dsizes_xout == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_xin-1; i++ ) {
    dsizes_xout[i] = dsizes_xin[i];
  }
  dsizes_xout[ndims_xin-1] = npout;

  size_xout   = size_leftmost * npout;

  if(type_xin  != NCL_double && type_pin != NCL_double &&
     type_pout != NCL_double) {

    type_xout = NCL_float;

    xout     = (void*)calloc(size_xout,sizeof(float));
    tmp_xout = (double*)calloc(npout,sizeof(double));
    if(xout == NULL || tmp_xout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_xout = NCL_double;
    xout = (void*)calloc(size_xout,sizeof(double));
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
    if(ndims_pin > 1) {
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
    if(ndims_pout > 1) {
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
        coerce_output_float_only(xout,tmp_xout,npout,index_out);
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
    ret = NclReturnValue(xout,ndims_xin,dsizes_xout,&missing_rx,
                          NCL_float,0);
  }
  else {
/*
 * Return double values with missing value set.
 */
    ret = NclReturnValue(xout,ndims_xin,dsizes_xout,&missing_dx,
                          NCL_double,0);
  }
  NclFree(dsizes_xout);
  return(ret);
}

#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dint2p,DINT2P)(double *,double *,double *,double *,
                                   double *,double *,int *,double *,
                                   double *,int *,int *,double *,int*);

NhlErrorTypes int2p_W( void )
{
/*
 * Input array variables
 */
  void *pin, *xin, *pout;
  double *tmp_pin = NULL;
  double *tmp_xin = NULL;
  double *tmp_pout = NULL;
  int ndims_pin;
  ng_size_t dsizes_pin[NCL_MAX_DIMENSIONS];
  NclScalar missing_pin, missing_xin, missing_dx;
  int has_missing_pin, has_missing_xin;
  int ndims_xin;
  ng_size_t dsizes_xin[NCL_MAX_DIMENSIONS];
  int ndims_pout;
  ng_size_t dsizes_pout[NCL_MAX_DIMENSIONS];
  int *linlog;
  NclBasicDataTypes type_pin, type_xin, type_pout;
/*
 * work arrays
 */
  double *p, *x, *p2, *x2;
/*
 * output variable 
 */
  void *xout;
  double *tmp_xout = NULL;
  ng_size_t size_xout, size_leftmost, *dsizes_xout;
  NclBasicDataTypes type_xout;
  NclScalar missing_xout;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, index_in, index_out;
  ng_size_t npin, npout;
  int ier = 0, ret, inpin, inpout;
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: pin must either be a one-dimensional array or an array the same size as xin");
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

/*
 * Test dimension sizes.
 */
  if((npin > INT_MAX) || (npout > INT_MAX)){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: npin and/or npout is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpin = (int) npin;
  inpout = (int) npout;

  if(ndims_pout != ndims_xin && ndims_pout != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: pout must either be a one-dimensional array or an array with the same number of dimensions as xin");
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
  dsizes_xout = (ng_size_t*)calloc(ndims_xin,sizeof(ng_size_t));
  if(dsizes_xout == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_xin-1; i++ ) {
    dsizes_xout[i] = dsizes_xin[i];
  }
  dsizes_xout[ndims_xin-1] = npout;

  size_xout   = size_leftmost * npout;

/*
 * Set type for return array.
 *
 * We used to check the types of xin, pin, and pout. Dennis asked
 * that we change this in V6.1.0 so that it only checks xin. 
 *
 * See JIRA NCL-1201.
 */
  if(type_xin != NCL_double) {
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
 * Check for missing values.
 */
  if(has_missing_xin) {
    coerce_missing(type_xin,has_missing_xin,&missing_xin,&missing_dx, 
                   NULL);
 }
  else if(has_missing_pin) {
    coerce_missing(type_pin,has_missing_pin,&missing_pin,&missing_dx,
                   NULL);
  }
  else {
/*
 * Assign a default missing value.
 */ 
    if(type_xout == NCL_float) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
  if(type_xout == NCL_float) {
    missing_xout.floatval = (float)missing_dx.doubleval;
  }
  else {
    missing_xout.doubleval = missing_dx.doubleval;
  }
/*
 * Allocate space for work arrays.
 */
  p  = (double*)calloc(npin,sizeof(double));
  x  = (double*)calloc(npin,sizeof(double));
  p2 = (double*)calloc(npin,sizeof(double));
  x2 = (double*)calloc(npin,sizeof(double));
  if (p == NULL || x == NULL || p2 == NULL || x2 == NULL) {
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

    NGCALLF(dint2p,DINT2P)(tmp_pin,tmp_xin,&p[0],&x[0],&p2[0],&x2[0],
                           &inpin,tmp_pout,tmp_xout,&inpout,linlog,
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
  NclFree(p2);
  NclFree(x2);
  if(type_xin  != NCL_double) NclFree(tmp_xin);
  if(type_pin  != NCL_double) NclFree(tmp_pin);
  if(type_pout != NCL_double) NclFree(tmp_pout);
  if(type_xout != NCL_double) NclFree(tmp_xout);

/*
 * Return values.
 */
  ret = NclReturnValue(xout,ndims_xin,dsizes_xout,&missing_xout,
                       type_xout,0);
  NclFree(dsizes_xout);
  return(ret);
}

NhlErrorTypes int2p_n_W( void )
{
/*
 * Input array variables
 */
  void *pin, *xin, *pout;
  double *tmp_pin, *tmp_xin, *tmp_pout;
  int *dim;
  int ndims_pin;
  ng_size_t dsizes_pin[NCL_MAX_DIMENSIONS];
  NclScalar missing_pin, missing_xin, missing_dx;
  int has_missing_pin, has_missing_xin;
  int ndims_xin;
  ng_size_t dsizes_xin[NCL_MAX_DIMENSIONS];
  int ndims_pout;
  ng_size_t dsizes_pout[NCL_MAX_DIMENSIONS];
  int *linlog;
  NclBasicDataTypes type_pin, type_xin, type_pout;
/*
 * work arrays
 */
  double *p, *x, *p2, *x2;
/*
 * output variable 
 */
  void *xout;
  double *tmp_xout;
  ng_size_t size_xout, size_leftmost, size_rightmost, *dsizes_xout;
  NclBasicDataTypes type_xout;
  NclScalar missing_xout;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t i, j, npin, npout;
  int ier = 0, ret, inpin, inpout;
  ng_size_t nrni, nrno, index_nri, index_nro, index_in, index_out;
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
          5,
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
          5,
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
          5,
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
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Retrieve argument #5
 */
  dim = (int*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_xin) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Invalid dimension index for pressure dimension");
    return(NhlFATAL);
  }

/*
 * Check dimension sizes for pin and xin. If both of them are multiple
 * dimensions, then they must be the same. Otherwise, pin must be the
 * same length as the dim-th dimension of xin.
 */
  npin = dsizes_xin[*dim];

  if (npin < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: The input level dimension must be at least two");
    return(NhlFATAL);
  }

  if(ndims_pin != ndims_xin && ndims_pin != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: pin must either be a one-dimensional array or an array the same size as xin");
    return(NhlFATAL);
  }

  if(ndims_pin == ndims_xin) {
    for( i = 0; i < ndims_pin; i++ ) {
      if (dsizes_pin[i] != dsizes_xin[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: If xin and pin have the same number of dimensions, then they must be the same dimension sizes");
        return(NhlFATAL);
      }
    }
  }
  else {
    if (dsizes_pin[0] != npin) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: If pin is a one-dimensional array, then it must be the same size as the level dimension of xin");
      return(NhlFATAL);
    }
  }

/*
 * Check dimension sizes for pout. If it is multi-dimensional, then
 * it must be the same as all but the level dimension of xin.
 */
  if(ndims_pout != ndims_xin && ndims_pout != 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: pout must either be a one-dimensional array or an array with the same number of dimensions as xin");
    return(NhlFATAL);
  }

  if(ndims_pout > 1) {
    for( i = 0; i < *dim; i++ ) {
      if (dsizes_pout[i] != dsizes_xin[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: If xin and pout have the same number of dimensions, then all but their level dimension must be the same size");
        return(NhlFATAL);
      }
    }
    for( i = *dim+1; i < ndims_pout; i++ ) {
      if (dsizes_pout[i] != dsizes_xin[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: If xin and pout have the same number of dimensions, then all but their level dimension must be the same size");
        return(NhlFATAL);
      }
    }
    npout = dsizes_pout[*dim];
  }
  else { 
    npout = dsizes_pout[0];
  }

/*
 * Test dimension sizes.
 */
  if((npin > INT_MAX) || (npout > INT_MAX)){
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: npin and/or npout is greater than INT_MAX");
    return(NhlFATAL);
  }
  inpin = (int) npin;
  inpout = (int) npout;

/*
 * Calculate the size of the leftmost dimensions of xin (if any).
 */
  size_rightmost = size_leftmost = 1;
  for( i =      0; i < *dim;      i++ ) size_leftmost  *= dsizes_xin[i];
  for( i = *dim+1; i < ndims_xin; i++ ) size_rightmost *= dsizes_xin[i];

/*
 * Create temporary arrays to coerce input to double, if necessary.
 * Note that because these arrays are not necessarily continguous
 * in memory, we have to make a copy of them regardless if they are
 * already double precision.
 */
  tmp_xin = (double*)calloc(npin,sizeof(double));
  if( tmp_xin == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate memory for coercing xin array to double precision");
    return(NhlFATAL);
  }
  if(ndims_pin == 1) {
    tmp_pin = coerce_input_double(pin,type_pin,npin,0,NULL,NULL);
  }
  else {
    tmp_pin = (double*)calloc(npin,sizeof(double));
    if( tmp_pin == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate memory for coercing pin array to double precision");
      return(NhlFATAL);
    }
  }
  if(ndims_pout == 1) {
    tmp_pout = coerce_input_double(pout,type_pout,npout,0,NULL,NULL);
  }
  else {
    tmp_pout = (double*)calloc(npout,sizeof(double));
    if( tmp_pout == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate memory for coercing pout array to double precision");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for output.
 */
  dsizes_xout = (ng_size_t*)calloc(ndims_xin,sizeof(ng_size_t));
  if(dsizes_xout == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
/*
 * Set dimension sizes for return array.
 */
  for(i = 0; i < ndims_xin; i++ ) dsizes_xout[i] = dsizes_xin[i];
  dsizes_xout[*dim] = npout;   /* Replace the one dimension with npout */

  size_xout   = size_leftmost * size_rightmost * npout;

/*
 * Set type for return array.
 *
 * We used to check the types of xin, pin, and pout. Dennis asked
 * that we change this in V6.1.0 so that it only checks xin. 
 *
 * See JIRA NCL-1201.
 */
  if(type_xin != NCL_double) {
    type_xout = NCL_float;

    xout     = (void*)calloc(size_xout,sizeof(float));
    tmp_xout = (double*)calloc(npout,sizeof(double));
  }
  else {
    type_xout = NCL_double;
    xout     = (void*)calloc(size_xout,sizeof(double));
    tmp_xout = (double*)calloc(npout,sizeof(double));
  }
  if(xout == NULL || tmp_xout == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if(has_missing_xin) {
    coerce_missing(type_xin,has_missing_xin,&missing_xin,&missing_dx,
                   NULL);
  }
  else if(has_missing_pin) {
    coerce_missing(type_pin,has_missing_pin,&missing_pin,&missing_dx,
                   NULL);
  }
  else {
/*
 * Assign a default missing value.
 */ 
    if(type_xout == NCL_float) {
      missing_dx.doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
    }
    else {
      missing_dx.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
    }
  }
  if(type_xout == NCL_float) {
    missing_xout.floatval = (float)missing_dx.doubleval;
  }
  else {
    missing_xout.doubleval = missing_dx.doubleval;
  }

/*
 * Allocate space for work arrays.
 */
  p  = (double*)calloc(npin,sizeof(double));
  x  = (double*)calloc(npin,sizeof(double));
  p2 = (double*)calloc(npin,sizeof(double));
  x2 = (double*)calloc(npin,sizeof(double));
  if (p == NULL || x == NULL || p2 == NULL || x2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"int2p_n: Unable to allocate space for work arrays" );
    return(NhlFATAL);
  }

/*
 * Call the Fortran version of this routine.
 *
 */
  nrni = size_rightmost * npin;
  nrno = size_rightmost * npout;
  for( i = 0; i < size_leftmost; i++ ) {
    index_nri = i*nrni;
    index_nro = i*nrno;
    for( j = 0; j < size_rightmost; j++ ) {
      index_in  = index_nri + j;
      index_out = index_nro + j;
      if(ndims_pin > 1) {
/*
 * Coerce npin subsection of pin (tmp_pin) to double.
 */
        coerce_subset_input_double_step(pin,tmp_pin,index_in,
                                        size_rightmost,type_pin,
                                        npin,0,NULL,NULL);
        coerce_subset_input_double_step(xin,tmp_xin,index_in,
                                        size_rightmost,type_xin,
                                        npin,0,NULL,NULL);
      }
      else {
/* 
 * pin is 1D, so do xin only.
 */
        coerce_subset_input_double_step(xin,tmp_xin,index_in,
                                        size_rightmost,type_xin,
                                        npin,0,NULL,NULL);
      }
      if(ndims_pout > 1) {
/*
 * Coerce npout subsection of pout (tmp_pout) to double.
 */
        coerce_subset_input_double_step(pout,tmp_pout,index_out,
                                        size_rightmost,type_pout,
                                        npout,0,NULL,NULL);
      }
      NGCALLF(dint2p,DINT2P)(tmp_pin,tmp_xin,&p[0],&x[0],&p2[0],&x2[0],
                             &inpin,tmp_pout,tmp_xout,&inpout,linlog,
                             &missing_dx.doubleval,&ier);

      if (ier) {
        if (ier >= 1000) nmiss++;
        else             nmono++;
        set_subset_output_missing_step(xout,index_out,size_rightmost,
                                       type_xout,npout,missing_dx.doubleval);
      }
      else {
/*
 * Copy output values from temporary tmp_xout to xout.
 */
        coerce_output_float_or_double_step(xout,tmp_xout,type_xout,npout,
                                           index_out,size_rightmost);
      }
    }
  }
  if (nmiss) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"int2p_n: %d input array(s) contained all missing data. No interpolation performed on these arrays",nmiss);
  }
  if (nmono) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"int2p_n: %d pin array(s) were in a different order than the corresponding pout array(s). No interpolation performed on these arrays",nmono);
  }
/*
 * Free memory.
 */
  NclFree(p);
  NclFree(x);
  NclFree(p2);
  NclFree(x2);
  NclFree(tmp_xin);
  if(ndims_pin  > 1 || type_pin  != NCL_double) NclFree(tmp_pin);
  if(ndims_pout > 1 || type_pout != NCL_double) NclFree(tmp_pout);
  NclFree(tmp_xout);

/*
 * Return values.
 */
  ret = NclReturnValue(xout,ndims_xin,dsizes_xout,&missing_xout,type_xout,0);
  NclFree(dsizes_xout);
  return(ret);
}

#include <stdio.h>
#include <string.h>
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

NhlErrorTypes rhomb_trunC_W( void )
{
/*
 * Input array variables
 */
  float *ab, *a, *b, *ab2;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  int nt, m, n;
  int *T;
/*
 * various
 */
  int i, j, size, nbytes, start;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (float*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          NULL,
          2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       1,
       2,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);
/*
 * Get last two dimensions of ab.
 */
  m = dsizes_ab[ndims_ab-2];
  n = dsizes_ab[ndims_ab-1];

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions and first dimension.
 */
  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);
/*
 * Create space for a, b arrays
 */
  a   = (float*)calloc(n*m*sizeof(float),1);
  b   = (float*)calloc(n*m*sizeof(float),1);
  ab2 = (float*)calloc(2*nt*n*m*sizeof(float),1);
  if( a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  if( ab2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call Fortran function
 */
  j      = 0;
  size   = n*m;
  nbytes = size*sizeof(float);
  start  = nt*n*m;

  for(i = 0; i < nt; i++) {
	memcpy(&a[0],&ab[j],      nbytes);
	memcpy(&b[0],&ab[start+j],nbytes);

    NGCALLF(rhombtrunc,RHOMBTRUNC)(&n,&m,&a[0],&b[0],T);

	memcpy(&ab2[j],      &a[0],nbytes);
	memcpy(&ab2[start+j],&b[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  free(a);
  free(b);
/*
 * Return
 */
  return(NclReturnValue((void*)ab2,ndims_ab,dsizes_ab,NULL,NCL_float,0));
}


NhlErrorTypes tri_trunC_W( void )
{
/*
 * Input array variables
 */
  float *ab, *a, *b, *ab2;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  int nt, m, n;
  int *T;
/*
 * various
 */
  int i, j, size, nbytes, start;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  ab = (float*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          NULL,
          2);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if(ndims_ab < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: The input array must be at least 3-dimensional");
    return(NhlFATAL);
  }
  if(dsizes_ab[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: The first dimension of the input array must be 2");
    return(NhlFATAL);
  }
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       1,
       2,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);
/*
 * Get last two dimensions of ab.
 */
  m = dsizes_ab[ndims_ab-2];
  n = dsizes_ab[ndims_ab-1];

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions and first dimension.
 */
  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);
/*
 * Create space for a, b arrays
 */
  a   = (float*)calloc(n*m*sizeof(float),1);
  b   = (float*)calloc(n*m*sizeof(float),1);
  ab2 = (float*)calloc(2*nt*n*m*sizeof(float),1);
  if( a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  if( ab2 == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Call Fortran function
 */
  j      = 0;
  size   = n*m;
  nbytes = size*sizeof(float);
  start  = nt*n*m;

  for(i = 0; i < nt; i++) {
	memcpy(&a[0],&ab[j],      nbytes);
	memcpy(&b[0],&ab[start+j],nbytes);

    NGCALLF(tritrunc,TRITRUNC)(&n,&m,&a[0],&b[0],T);

	memcpy(&ab2[j],      &a[0],nbytes);
	memcpy(&ab2[start+j],&b[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  free(a);
  free(b);
/*
 * Return
 */
  return(NclReturnValue((void*)ab2,ndims_ab,dsizes_ab,NULL,NCL_float,0));
}


NhlErrorTypes rhomb_trunc_W( void )
{
/*
 * Input array variables
 */
  float *a, *b, *a2, *b2;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  int nt, m, n;
  int *T;
/*
 * various
 */
  int i, j, size, nbytes;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (float*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          NULL,
          2);

  b = (float*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          NULL,
          2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: The input arrays must have the same number of dimensions and be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
	if(dsizes_a[i] != dsizes_b[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: The input arrays must have the same dimension sizes");
	  return(NhlFATAL);
	}
  }
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);
/*
 * Get last two dimensions of a and b.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
/*
 * Create space for a, b arrays
 */
  a2 = (float*)calloc(n*m*sizeof(float),1);
  b2 = (float*)calloc(n*m*sizeof(float),1);
  if( a2 == NULL || b2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call Fortran function.
 */
  j      = 0;
  size   = n * m;
  nbytes = size*sizeof(float);

  for(i = 0; i < nt; i++) {
	memcpy(&a2[0],&a[j],nbytes);
	memcpy(&b2[0],&b[j],nbytes);

    NGCALLF(rhombtrunc,RHOMBTRUNC)(&n,&m,&a2[0],&b2[0],T);

	memcpy(&a[j],&a2[0],nbytes);
	memcpy(&b[j],&b2[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  free(a2);
  free(b2);
/*
 * Return
 */
  return(NhlNOERROR);
}


NhlErrorTypes tri_trunc_W( void )
{
/*
 * Input array variables
 */
  float *a, *b, *a2, *b2;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  int nt, m, n;
  int *T;
/*
 * various
 */
  int i, j, size, nbytes;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  a = (float*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          NULL,
          2);

  b = (float*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          NULL,
          2);
/*
 * The grids coming in must be at least 2-dimensional.
 */
  if(ndims_a != ndims_b || ndims_a < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: The input arrays must have the same number of dimensions and be at least 2-dimensional");
    return(NhlFATAL);
  }
  for( i = 0; i < ndims_a; i++ ) {
	if(dsizes_a[i] != dsizes_b[i]) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: The input arrays must have the same dimension sizes");
	  return(NhlFATAL);
	}
  }
/*
 * Get T
 */
  T = (int*)NclGetArgValue(
       2,
       3,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       2);
/*
 * Get last two dimensions of a and b.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];

/*
 * Compute the total number of elements in our array, minus last
 * two dimensions.
 */
  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
/*
 * Create space for a, b arrays
 */
  a2 = (float*)calloc(n*m*sizeof(float),1);
  b2 = (float*)calloc(n*m*sizeof(float),1);
  if( a2 == NULL || b2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call Fortran function.
 */
  j      = 0;
  size   = n * m;
  nbytes = size*sizeof(float);

  for(i = 0; i < nt; i++) {
	memcpy(&a2[0],&a[j],nbytes);
	memcpy(&b2[0],&b[j],nbytes);

    NGCALLF(tritrunc,TRITRUNC)(&n,&m,&a2[0],&b2[0],T);

	memcpy(&a[j],&a2[0],nbytes);
	memcpy(&b[j],&b2[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  free(a2);
  free(b2);
/*
 * Return
 */
  return(NhlNOERROR);
}


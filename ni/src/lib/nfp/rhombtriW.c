#include <stdio.h>
#include <string.h>
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

extern void NGCALLF(drhombtrunc,DRHOMBTRUNC)(int*,int*,double*,double*,
                                             int*);
extern void NGCALLF(dtritrunc,DTRITRUNC)(int*,int*,double*,double*,int*);

NhlErrorTypes rhomb_trunC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *dab, *a, *b;
  float *rab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab;
  int nt, m, n, total_size_ab;
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
  ab = (void*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          &type_ab,
          2);
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
 * Compute the total number of elements in our array.
 */
  m  = dsizes_ab[ndims_ab-2];
  n  = dsizes_ab[ndims_ab-1];

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);

  total_size_ab = 2*nt*n*m;
/*
 * Coerce ab to double no matter what, because we need an extra copy of this
 * array anyway.
 */
  dab = (double*)NclMalloc(sizeof(double)*total_size_ab);
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for coercing ab array to double precision");
    return(NhlFATAL);
  }
  _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             dab,
             ab,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ab)));
/*
 * Create space for a and b arrays
 */
  a   = (double*)calloc(n*m*sizeof(double),1);
  b   = (double*)calloc(n*m*sizeof(double),1);
  if( a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Call Fortran function
 */
  j      = 0;
  size   = n*m;
  nbytes = size*sizeof(double);
  start  = nt*n*m;

  for(i = 0; i < nt; i++) {
/*
 * Copy ab array into individual a and b arrays.
 */
    memcpy(&a[0],&dab[j],      nbytes);
    memcpy(&b[0],&dab[start+j],nbytes);

    NGCALLF(drhombtrunc,DRHOMBTRUNC)(&n,&m,&a[0],&b[0],T);
/*
 * Copy a and b arrays back into ab array.
 */
    memcpy(&dab[j],      &a[0],nbytes);
    memcpy(&dab[start+j],&b[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  NclFree(a);
  NclFree(b);
/*
 * Return values. 
 */
  if(type_ab != NCL_double) {
/*
 * Input is not double, so return float values.
 *
 * First copy double values to float values.
 */
    rab = (float*)NclMalloc(sizeof(float)*total_size_ab);
    if( rab == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunC: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_ab; i++ ) rab[i] = (float)dab[i];

/*
 * Free double precision values.
 */
    NclFree(dab);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
  }
  else {
/*
 * Return double values.
 */
    return(NclReturnValue((void*)dab,ndims_ab,dsizes_ab,NULL,NCL_double,0));
  }
}


NhlErrorTypes tri_trunC_W( void )
{
/*
 * Input array variables
 */
  void *ab;
  double *dab, *a, *b;
  float *rab;
  int ndims_ab, dsizes_ab[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_ab;
  int nt, m, n, total_size_ab;
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
  ab = (void*)NclGetArgValue(
          0,
          2,
          &ndims_ab, 
          dsizes_ab,
          NULL,
          NULL,
          &type_ab,
          2);
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
 * Compute the total number of elements in our array.
 */
  m = dsizes_ab[ndims_ab-2];
  n = dsizes_ab[ndims_ab-1];

  nt = 1;
  for(i = 1; i < ndims_ab-2; nt*=dsizes_ab[i],i++);
  total_size_ab = 2*nt*n*m;
/*
 * Coerce ab to double no matter what, because we need an extra copy of this
 * array anyway.
 */
  dab = (double*)NclMalloc(sizeof(double)*total_size_ab);
  if( dab == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for coercing ab array to double precision");
    return(NhlFATAL);
  }
  _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             dab,
             ab,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_ab)));
/*
 * Create space for a and b arrays
 */
  a   = (double*)calloc(n*m*sizeof(double),1);
  b   = (double*)calloc(n*m*sizeof(double),1);
  if( a == NULL || b == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Call Fortran function
 */
  j      = 0;
  size   = n*m;
  nbytes = size*sizeof(double);
  start  = nt*n*m;

  for(i = 0; i < nt; i++) {
/*
 * Copy ab array into individual a and b arrays.
 */
    memcpy(&a[0],&dab[j],      nbytes);
    memcpy(&b[0],&dab[start+j],nbytes);

    NGCALLF(dtritrunc,DTRITRUNC)(&n,&m,&a[0],&b[0],T);
/*
 * Copy a and b arrays back into ab array.
 */
    memcpy(&dab[j],      &a[0],nbytes);
    memcpy(&dab[start+j],&b[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  NclFree(a);
  NclFree(b);
/*
 * Return values. 
 */
  if(type_ab != NCL_double) {
/*
 * Input is not double, so return float values.
 *
 * First copy double values to float values.
 */
    rab = (float*)NclMalloc(sizeof(float)*total_size_ab);
    if( rab == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunC: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_ab; i++ ) rab[i] = (float)dab[i];
/*
 * Free double precision values.
 */
    NclFree(dab);
/*
 * Return float values.
 */
    return(NclReturnValue((void*)rab,ndims_ab,dsizes_ab,NULL,NCL_float,0));
  }
  else {
/*
 * Return double values.
 */
    return(NclReturnValue((void*)dab,ndims_ab,dsizes_ab,NULL,NCL_double,0));
  }
}

NhlErrorTypes rhomb_trunc_W( void )
{
/*
 * Input array variables
 */
  void *a, *b;
  double *da, *db, *a2, *b2;
  float *ra, *rb;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
  int nt, m, n, total_size_ab;
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
  a = (void*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          2);

  b = (void*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          2);
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
 * Compute the total number of elements in our array.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];

  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
  total_size_ab = nt*n*m;
/*
 * a and b must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: a and b must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Coerce a to double.
 */
  if(type_a != NCL_double) {
    da = (double*)NclMalloc(sizeof(double)*total_size_ab);
    if( da == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             da,
             a,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_a)));
  }
  else {
/*
 * Input is already double.
 */
    da = (double*)a;
  }
/*
 * Coerce b to double.
 */
  if(type_b != NCL_double) {
    db = (double*)NclMalloc(sizeof(double)*total_size_ab);
    if( db == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             db,
             b,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_b)));
  }
  else {
/*
 * Input is already double.
 */
    db = (double*)b;
  }
/*
 * Create space for a, b arrays
 */
  a2 = (double*)calloc(n*m*sizeof(double),1);
  b2 = (double*)calloc(n*m*sizeof(double),1);
  if( a2 == NULL || b2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"rhomb_trunc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call Fortran function.
 */
  j      = 0;
  size   = n * m;
  nbytes = size*sizeof(double);

  for(i = 0; i < nt; i++) {
/*
 * Copy original big a and b arrays into smaller a and b arrays.
 */
    memcpy(&a2[0],&da[j],nbytes);
    memcpy(&b2[0],&db[j],nbytes);

    NGCALLF(drhombtrunc,DRHOMBTRUNC)(&n,&m,&a2[0],&b2[0],T);
/*
 * Copy smaller a and b arrays back into original big a and b arrays.
 */
    memcpy(&da[j],&a2[0],nbytes);
    memcpy(&db[j],&b2[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  NclFree(a2);
  NclFree(b2);
/*
 * If returning float values, we need to copy the coerced float values
 * back to the original location of a and b.  Do this by creating a pointer
 * of type float that points to the original location, and then loop through
 * the values and do the coercion.
 */
  if(type_a == NCL_float) {
    ra = (float*)a;     /* Float pointer to original a array */
    for( i = 0; i < total_size_ab; i++ ) ra[i]  = (float)da[i];
    NclFree(da);   /* Free up the double array */
  }

  if(type_b == NCL_float) {
    rb = (float*)b;     /* Float pointer to original b array */
    for( i = 0; i < total_size_ab; i++ ) rb[i]  = (float)db[i];
    NclFree(db);   /* Free up the double array */
  }

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
  void *a, *b;
  double *da, *db, *a2, *b2;
  float *ra, *rb;
  int ndims_a, dsizes_a[NCL_MAX_DIMENSIONS];
  int ndims_b, dsizes_b[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_a, type_b;
  int nt, m, n, total_size_ab;
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
  a = (void*)NclGetArgValue(
          0,
          3,
          &ndims_a, 
          dsizes_a,
          NULL,
          NULL,
          &type_a,
          2);

  b = (void*)NclGetArgValue(
          1,
          3,
          &ndims_b,
          dsizes_b,
          NULL,
          NULL,
          &type_b,
          2);
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
 * Compute the total number of elements in our array.
 */
  m = dsizes_a[ndims_a-2];
  n = dsizes_a[ndims_a-1];

  nt = 1;
  for(i = 0; i < ndims_a-2; nt*=dsizes_a[i],i++);
  total_size_ab = nt*n*m;
/*
 * a and b must be float or double.
 */
  if((type_a != NCL_float && type_a != NCL_double) ||
     (type_b != NCL_float && type_b != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: a and b must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Coerce a to double.
 */
  if(type_a != NCL_double) {
    da = (double*)NclMalloc(sizeof(double)*total_size_ab);
    if( da == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: Unable to allocate memory for coercing a array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             da,
             a,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_a)));
  }
  else {
/*
 * Input is already double.
 */
    da = (double*)a;
  }
/*
 * Coerce b to double.
 */
  if(type_b != NCL_double) {
    db = (double*)NclMalloc(sizeof(double)*total_size_ab);
    if( db == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: Unable to allocate memory for coercing b array to double precision");
      return(NhlFATAL);
    }
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
             db,
             b,
             total_size_ab,
             NULL,
             NULL,
             _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_b)));
  }
  else {
/*
 * Input is already double.
 */
    db = (double*)b;
  }
/*
 * Create space for a, b arrays
 */
  a2 = (double*)calloc(n*m*sizeof(double),1);
  b2 = (double*)calloc(n*m*sizeof(double),1);
  if( a2 == NULL || b2 == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tri_trunc: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call Fortran function.
 */
  j      = 0;
  size   = n * m;
  nbytes = size*sizeof(double);

  for(i = 0; i < nt; i++) {
/*
 * Copy original big a and b arrays into smaller a and b arrays.
 */
    memcpy(&a2[0],&da[j],nbytes);
    memcpy(&b2[0],&db[j],nbytes);

    NGCALLF(dtritrunc,DTRITRUNC)(&n,&m,&a2[0],&b2[0],T);
/*
 * Copy smaller a and b arrays back into original big a and b arrays.
 */
    memcpy(&da[j],&a2[0],nbytes);
    memcpy(&db[j],&b2[0],nbytes);

    j += size;
  }

/*
 * Free work arrays.
 */ 
  NclFree(a2);
  NclFree(b2);
/*
 * If returning float values, we need to copy the coerced float values
 * back to the original location of a and b.  Do this by creating a pointer
 * of type float that points to the original location, and then loop through
 * the values and do the coercion.
 */
  if(type_a == NCL_float) {
    ra = (float*)a;     /* Float pointer to original a array */
    for( i = 0; i < total_size_ab; i++ ) ra[i]  = (float)da[i];
    NclFree(da);   /* Free up the double array */
  }

  if(type_b == NCL_float) {
    rb = (float*)b;     /* Float pointer to original b array */
    for( i = 0; i < total_size_ab; i++ ) rb[i]  = (float)db[i];
    NclFree(db);   /* Free up the double array */
  }

/*
 * Return
 */
  return(NhlNOERROR);
}


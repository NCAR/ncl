#include <string.h>
#include <stdio.h>
#include "wrapper.h"
#include "DataSupport.h"
#include <ncarg/c.h>

extern NGCALLF(dimgbits,DIMGBITS)(int *, int *, int *, int *, int *,
                                  int *, int *);

NhlErrorTypes dim_gbits_W( void )
{
/*  
 * Input.
 */
  void *npack;
  int ndims_npack, dsizes_npack[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_npack;
  int *ibit, *nbits, *nskip, *iter;
  int tmp_ibit, tmp_nskip;
/*  
 * Output.
 */
  void *isam;
  int *tmp_isam, *dsizes_isam;

/*
 * Various.
 */
  int i, j, n, size_leftmost, size_isam, *tmp_npack;
  int size_nbytes_type, size_int_type;
  int index_npack = 0, index_isam = 0;
  NclTypeClass typeclass_npack;
/*
 * Retrieve first argument.
 */
  npack = (void*)NclGetArgValue(0,
                                5,
                                &ndims_npack,
                                dsizes_npack,
                                NULL,
                                NULL,
                                &type_npack,
                                2);

/*
 * Check type of npack.
 */
  if(type_npack != NCL_int && type_npack != NCL_byte &&
     type_npack != NCL_short) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: npack must either be of type byte, short, or integer");
      return(NhlFATAL);
  }

/*
 * typeclass_npack is what allows us to get the size of the type of npack.
 */
  typeclass_npack = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_npack)));

  size_nbytes_type = typeclass_npack->type_class.size;
  size_int_type    = ((NclTypeClass)nclTypeintClass)->type_class.size;

/*
 * Retrieve rest of arguments.
 */
  ibit = (int*)NclGetArgValue(1,
                              5,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              2);

  nbits = (int*)NclGetArgValue(2,
                               5,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               2);

  nskip = (int*)NclGetArgValue(3,
                               5,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               2);

  iter = (int*)NclGetArgValue(4,
                              5,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              2);

/*
 * Compute total number of elements in npack and isam arrays.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_npack-1; i++) {
    size_leftmost *= dsizes_npack[i];
  }
  n         = dsizes_npack[ndims_npack-1];
  size_isam = *iter * size_leftmost;

/*
 * Allocate space for input/output arrays.
 */
  if(type_npack != NCL_int) {
    tmp_npack = (int*)calloc(n,sizeof(int));
    tmp_isam  = (int*)calloc(*iter,sizeof(int));
    tmp_ibit  = *ibit  + 8*(size_int_type - size_nbytes_type);
    tmp_nskip = *nskip + 8*(size_int_type - size_nbytes_type);

    if(tmp_npack == NULL || tmp_isam == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: Unable to allocate memory for temporary input/output arrays");
      return(NhlFATAL);
    }
    if(type_npack == NCL_byte) {
      isam = (void*)calloc(size_isam,sizeof(byte));
    }
    else {
      isam = (void*)calloc(size_isam,sizeof(short));
    }
  }
  else {
    tmp_ibit  = *ibit;
    tmp_nskip = *nskip;
    isam = (void*)calloc(size_isam,sizeof(int));
  }
  if(isam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate space for dimension sizes of output array.
 */
  dsizes_isam = (int*)calloc(ndims_npack,sizeof(int));
  if(dsizes_isam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: Unable to allocate memory for holding size of output array");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_npack-1; i++) {
    dsizes_isam[i] = dsizes_npack[i];
  }
  dsizes_isam[ndims_npack-1] = *iter;

/*
 * Call the Fortran routine. 
 */

  for(i = 1; i <= size_leftmost; i++) {
    if(type_npack != NCL_int) {
      _Nclcoerce((NclTypeClass)nclTypeintClass,
                 tmp_npack,
                 (void*)((char*)npack+index_npack*size_nbytes_type),
                 n,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_npack)));

    }
    else {
/*
 * npack is already an integer.
 */
      tmp_npack = &((int*)npack)[index_npack];
      tmp_isam  = &((int*)isam)[index_isam];
    }

    NGCALLF(dimgbits,DIMGBITS)(&n,tmp_npack,tmp_isam,&tmp_ibit,nbits,
			       &tmp_nskip,iter);

    if(type_npack == NCL_short) {
      for(j = 0; j < *iter; j++ ) {
        ((short*)isam)[index_isam+j] = (short)(tmp_isam[j]);
      }
    }
    else if(type_npack == NCL_byte) {
      for(j = 0; j < *iter; j++ ) {
        ((byte*)isam)[index_isam+j] = (byte)(tmp_isam[j]);
      }
    }
    index_npack += n;
    index_isam  += *iter;
  }

/*
 * Free memory.
 */
  if(type_npack != NCL_int) {
    NclFree(tmp_npack);
    NclFree(tmp_isam);
  }
/*
 * Return.
 */
  return(NclReturnValue( (void *) isam, ndims_npack, dsizes_isam, 
                         NULL, type_npack, 0));
}


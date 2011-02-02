#include <string.h>
#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(gbytes,GBYTES)(int *, int *, int *, int *, int *, int *);
extern void NGCALLF(sbytes,SBYTES)(int *, int *, int *, int *, int *, int *);
extern void NGCALLF(getbitsone,GETBITSONE)(int *, int *, int *, int *, int *);

NhlErrorTypes dim_gbits_W( void )
{
/*  
 * Input.
 */
  void *npack;
  int ndims_npack;
  ng_size_t dsizes_npack[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_npack;
  int *ibit, *nbits, *nskip, *iter;
/*  
 * Output.
 */
  void *isam;
  int *tmp_isam = NULL;
  ng_size_t *dsizes_isam;

/*
 * Various.
 */
  ng_size_t i, j;
  ng_size_t size_leftmost, size_isam;
  int n, tmp_ibit, tmp_nbits, tmp_nskip;
  int *tmp_npack = NULL;
  int *tmp_npack2 = NULL;

  int size_npack_type, size_int_type;
  int ret;
  ng_size_t index_npack = 0, index_isam = 0;
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
                                DONT_CARE);

/*
 * Check type of npack.
 */
  if(type_npack != NCL_byte && type_npack != NCL_ubyte && 
     type_npack != NCL_short && type_npack != NCL_ushort && 
     type_npack != NCL_int) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: npack must either be of type byte, unsigned byte, short, unsigned short, or int");
      return(NhlFATAL);
  }

/*
 * typeclass_npack is what allows us to get the size of the type of npack.
 */
  typeclass_npack = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_npack)));

  size_npack_type = typeclass_npack->type_class.size;
  size_int_type   = ((NclTypeClass)nclTypeintClass)->type_class.size;

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
                              DONT_CARE);

  nbits = (int*)NclGetArgValue(2,
                               5,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);

  nskip = (int*)NclGetArgValue(3,
                               5,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               NULL,
                               DONT_CARE);

  iter = (int*)NclGetArgValue(4,
                              5,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              NULL,
                              DONT_CARE);

/*
 * Compute total number of elements in npack and isam arrays.
 */
  size_leftmost = 1;
  for(i = 0; i < ndims_npack-1; i++) {
    size_leftmost *= dsizes_npack[i];
  }
  size_isam = *iter * size_leftmost;

  if(dsizes_npack[ndims_npack-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: the rightmost dimension of npack is greater than INT_MAX");
    return(NhlFATAL);
  }
  n = (int)dsizes_npack[ndims_npack-1];

/*
 * Allocate space for input/output arrays.
 */
  if(type_npack != NCL_int) {
    tmp_npack = (int*)calloc(n,sizeof(int));
    tmp_npack2= (int*)calloc(n,sizeof(int));
    tmp_isam  = (int*)calloc(*iter,sizeof(int));

    if(tmp_npack2 == NULL || tmp_npack == NULL || tmp_isam == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: Unable to allocate memory for temporary input/output arrays");
      return(NhlFATAL);
    }
    if(type_npack == NCL_byte) {
      isam = (void*)calloc(size_isam,sizeof(byte));
    }
    else if(type_npack == NCL_ubyte) {
      isam = (void*)calloc(size_isam,sizeof(unsigned char));
    }
    else if(type_npack == NCL_short) {
      isam = (void*)calloc(size_isam,sizeof(short));
    }
    else {   /* if(type_npack == NCL_ushort) */
      isam = (void*)calloc(size_isam,sizeof(unsigned short));
    }
  }
  else {
    isam = (void*)calloc(size_isam,sizeof(int));
  }
  if(isam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_gbits: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate space for dimension sizes of output array.
 */
  dsizes_isam = (ng_size_t *)calloc(ndims_npack,sizeof(ng_size_t));
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
                 (void*)((char*)npack+index_npack*size_npack_type),
                 n,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_npack)));
/*
 * Run tmp_npack through sbytes to pack it into the leftmost bits of 
 * tmp_npack2.
 */ 
      tmp_ibit  = 0;
      tmp_nskip = 0;
      tmp_nbits = 8*size_npack_type;
      NGCALLF(sbytes,SBYTES)(tmp_npack2,tmp_npack,&tmp_ibit,
                             &tmp_nbits,&tmp_nskip,&n);
    }
    else {
/*
 * npack is already an integer.
 */
      tmp_npack2 = &((int*)npack)[index_npack];
      tmp_isam   = &((int*)isam)[index_isam];
    }

    NGCALLF(gbytes,GBYTES)(tmp_npack2,tmp_isam,ibit,nbits,nskip,iter);

    if(type_npack == NCL_byte) {
      for(j = 0; j < *iter; j++ ) {
        ((byte*)isam)[index_isam+j] = (byte)(tmp_isam[j]);
      }
    }
    else if(type_npack == NCL_ubyte) {
      for(j = 0; j < *iter; j++ ) {
        ((unsigned char*)isam)[index_isam+j] = (unsigned char)(tmp_isam[j]);
      }
    }
    else if(type_npack == NCL_short) {
      for(j = 0; j < *iter; j++ ) {
        ((short*)isam)[index_isam+j] = (short)(tmp_isam[j]);
      }
    }
    else if(type_npack == NCL_ushort) {
      for(j = 0; j < *iter; j++ ) {
        ((unsigned short*)isam)[index_isam+j] = (unsigned short)(tmp_isam[j]);
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
    NclFree(tmp_npack2);
    NclFree(tmp_isam);
  }
/*
 * Return.
 */
  ret = NclReturnValue( (void *) isam, ndims_npack, dsizes_isam, NULL,
                        type_npack, 0);
  NclFree(dsizes_isam);
  return(ret);
}

NhlErrorTypes getbitsone_W( void )
{
/*  
 * Input.
 */
  void *npack;
  int ndims_npack;
  ng_size_t dsizes_npack[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_npack;
/*  
 * Output.
 */
  void *isam;
  int *tmp_isam = NULL;
  ng_size_t *dsizes_isam;

/*
 * Various.
 */
  ng_size_t i, j;
  int size_npack, size_isam;
  int size_npack_type;
  int ibit, nbits, nskip;
  int ret;
  int *tmp_npack = NULL;
  int *tmp_npack2 = NULL;

  NclTypeClass typeclass_npack;

/*
 * Retrieve first argument.
 */
  npack = (void*)NclGetArgValue(0,
                                1,
                                &ndims_npack,
                                dsizes_npack,
                                NULL,
                                NULL,
                                &type_npack,
                                DONT_CARE);

/*
 * Check type of npack.
 */
  if(type_npack != NCL_byte && type_npack != NCL_ubyte && 
     type_npack != NCL_short && type_npack != NCL_ushort && 
     type_npack != NCL_int) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"getbitsone_gbits: npack must either be of type byte, unsigned byte, short, unsigned short, or int");
      return(NhlFATAL);
  }

/*
 * typeclass_npack is what allows us to get the size of the type of npack.
 */
  typeclass_npack = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_npack)));

  size_npack_type = typeclass_npack->type_class.size;

/*
 * Compute total number of elements in npack and isam arrays.
 */
  size_npack = 1;
  for(i = 0; i < ndims_npack; i++) {
    size_npack *= dsizes_npack[i];
  }
  size_isam = 8 * size_npack_type * size_npack;

/*
 * Allocate space for input/output arrays.
 */
  if(type_npack != NCL_int) {
    tmp_npack  = (int*)calloc(size_npack,sizeof(int));
    tmp_npack2 = (int*)calloc(size_npack,sizeof(int));
    tmp_isam   = (int*)calloc(size_isam,sizeof(int));

    if(tmp_npack2 == NULL || tmp_npack == NULL || tmp_isam == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"getbitsone: Unable to allocate memory for temporary input/output arrays");
      return(NhlFATAL);
    }
    if(type_npack == NCL_byte) {
      isam = (void*)calloc(size_isam,sizeof(byte));
    }
    else if(type_npack == NCL_ubyte) {
      isam = (void*)calloc(size_isam,sizeof(unsigned char));
    }
    else if(type_npack == NCL_short) {
      isam = (void*)calloc(size_isam,sizeof(short));
    }
    else {   /* if(type_npack == NCL_ushort) */
      isam = (void*)calloc(size_isam,sizeof(unsigned short));
    }
  }
  else {
    isam = (void*)calloc(size_isam,sizeof(int));
  }
  if(isam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"getbitsone: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate space for dimension sizes of output array.
 */
  dsizes_isam = (ng_size_t *)calloc(ndims_npack+1,sizeof(ng_size_t));
  if(dsizes_isam == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"getbitsone: Unable to allocate memory for holding size of output array");
    return(NhlFATAL);
  }

  for(i = 0; i < ndims_npack; i++) {
    dsizes_isam[i] = dsizes_npack[i];
  }
  dsizes_isam[ndims_npack] = 8 * size_npack_type;

/*
 * Call the Fortran routine. 
 */

  if(type_npack != NCL_int) {
    _Nclcoerce((NclTypeClass)nclTypeintClass,
               tmp_npack,
               npack,
               size_npack,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_npack)));
/*
 * Run tmp_npack through sbytes to pack it into the leftmost bits of 
 * tmp_npack2.
 */ 
    ibit  = 0;
    nskip = 0;
    nbits = 8*size_npack_type;
    NGCALLF(sbytes,SBYTES)(tmp_npack2,tmp_npack,&ibit,&nbits,&nskip,
                           &size_npack);
  }
  else {
/*
 * npack is already an integer.
 */
    tmp_npack2 = &((int*)npack)[0];
    tmp_isam   = &((int*)isam)[0];
  }

  ibit  = 0;
  nskip = 0;
  nbits = 1;
  NGCALLF(gbytes,GBYTES)(tmp_npack2,tmp_isam,&ibit,&nbits,&nskip,&size_isam);

  if(type_npack == NCL_byte) {
    for(j = 0; j < size_isam; j++ ) {
      ((byte*)isam)[j] = (byte)(tmp_isam[j]);
    }
  }
  else if(type_npack == NCL_ubyte) {
    for(j = 0; j < size_isam; j++ ) {
      ((unsigned char*)isam)[j] = (unsigned char)(tmp_isam[j]);
    }
  }
  else if(type_npack == NCL_short) {
    for(j = 0; j < size_isam; j++ ) {
      ((short*)isam)[j] = (short)(tmp_isam[j]);
    }
  }
  else if(type_npack == NCL_ushort) {
    for(j = 0; j < size_isam; j++ ) {
      ((unsigned short*)isam)[j] = (unsigned short)(tmp_isam[j]);
    }
  }

/*
 * Free memory.
 */
  if(type_npack != NCL_int) {
    NclFree(tmp_npack2);
    NclFree(tmp_npack);
    NclFree(tmp_isam);
  }
/*
 * Return.
 */
  ret = NclReturnValue( (void *) isam, ndims_npack+1, dsizes_isam, NULL,
                        type_npack, 0);
  NclFree(dsizes_isam);
  return(ret);

}


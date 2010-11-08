#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dpopremap,DPOPREMAP)(double *,double *,int *,int *,
                                         double *,int *,int *, int *, 
                                         int *, double *);

NhlErrorTypes pop_remap_W( void )
{
/*
 * Input variables
 */
  void *dst_array, *map_wts, *src_array;
  double *dst, *map, *src;
  int has_missing_src_array, *dst_add, *src_add;
  ng_size_t ndst, nlink, nw, nsrc;
  ng_size_t dsizes_dst_array[1];
  ng_size_t dsizes_map_wts[2];
  ng_size_t dsizes_src_array[1];
  ng_size_t dsizes_dst_add[1];
  ng_size_t dsizes_src_add[1];
  NclBasicDataTypes type_dst_array, type_map_wts, type_src_array;
  NclScalar missing_src_array, missing_dsrc_array;
  int indst, inlink, inw, insrc;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  dst_array = (void*)NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_dst_array,
          NULL,
          NULL,
          &type_dst_array,
          DONT_CARE);

  map_wts = (void*)NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_map_wts,
          NULL,
          NULL,
          &type_map_wts,
          DONT_CARE);

  dst_add = (int*)NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_dst_add,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  src_add = (int*)NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_src_add,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  src_array = (void*)NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_src_array,
          &missing_src_array,
          &has_missing_src_array,
          &type_src_array,
          DONT_CARE);
/*
 * Check type of dst_array.
 */
  if(type_dst_array != NCL_float && type_dst_array != NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pop_remap: dst_array must be of type float or double");
    return(NhlFATAL);
  }
/*
 * Check dimensions and calculate total size of arrays.
 */
  nlink = dsizes_map_wts[0];
  nw    = dsizes_map_wts[1];
  ndst  = dsizes_dst_array[0];
  nsrc  = dsizes_src_array[0];

  if( dsizes_dst_add[0] != nlink || dsizes_src_add[0] != nlink ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pop_remap: The size of the dst_add and src_add arrays must be the same as the first dimension of map_wts");
    return(NhlFATAL);
  }
  if((ndst > INT_MAX) || (nlink > INT_MAX) || (nw > INT_MAX) || (nsrc > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pop_remap: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  indst = (int) ndst;
  inlink = (int) nlink;
  inw = (int) nw;
  insrc = (int) nsrc;

/*
 * Check that src_array has a missing value set.
 */
  if(!has_missing_src_array) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,"pop_remap: No missing values are being set.\nDefault missing values will be used.\nBe careful of results.");
  }
  coerce_missing(type_src_array,has_missing_src_array,&missing_src_array,
                 &missing_dsrc_array,NULL);
/*
 * Coerce input to double.
 */
  map = coerce_input_double(map_wts,type_map_wts,nlink*nw,0,NULL,NULL);
  src = coerce_input_double(src_array,type_src_array,nsrc,
                            has_missing_src_array,&missing_src_array,NULL);

  if(map == NULL || src == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pop_remap: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Calloc space for output array if necessary.
 */
  if(type_dst_array == NCL_float) {
    dst = (double*)calloc(ndst,sizeof(double));
    if(dst == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"pop_remap: Unable to allocate memory for output array"); 
      return(NhlFATAL);
    }
  }
  else {
    dst = (double*)dst_array;
  }

/*
 * Call Fortran popremap.
 */
  NGCALLF(dpopremap,DPOPREMAP)(dst,map,dst_add,src_add,src,&indst,&inlink,&inw,
                               &insrc,&missing_dsrc_array.doubleval);

  if(type_dst_array == NCL_float) {
    coerce_output_float_only(dst_array,dst,ndst,0);
    NclFree(dst);
  }
  if(type_map_wts   != NCL_double) NclFree(map);
  if(type_src_array != NCL_double) NclFree(src);

  return(NhlNOERROR);
}


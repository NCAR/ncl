#include <stdio.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

extern void NGCALLF(popremap,POPREMAP)(float *,float *,int *,int *,float *,
                                       int *,int *, int *, int *, float *);

NhlErrorTypes pop_remap_W( void )
{
/*
 * Input variables
 */
  float *dst_array, *map_wts, *src_array, xmsg;
  int has_missing_src_array, *dst_add, *src_add, ndst, nlink, nw, nsrc;
  int ndims_dst_array, dsizes_dst_array[NCL_MAX_DIMENSIONS];
  int ndims_map_wts, dsizes_map_wts[NCL_MAX_DIMENSIONS];
  int ndims_src_array, dsizes_src_array[NCL_MAX_DIMENSIONS];
  int ndims_dst_add, dsizes_dst_add[NCL_MAX_DIMENSIONS];
  int ndims_src_add, dsizes_src_add[NCL_MAX_DIMENSIONS];
  NclScalar missing_src_array;
/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  dst_array = (float*)NclGetArgValue(
          0,
          5,
          &ndims_dst_array, 
          dsizes_dst_array,
          NULL,
          NULL,
          NULL,
          2);

  map_wts = (float*)NclGetArgValue(
          1,
          5,
          &ndims_map_wts,
          dsizes_map_wts,
          NULL,
          NULL,
          NULL,
          2);

  dst_add = (int*)NclGetArgValue(
          2,
          5,
          &ndims_dst_add, 
          dsizes_dst_add,
          NULL,
          NULL,
          NULL,
          2);

  src_add = (int*)NclGetArgValue(
          3,
          5,
          &ndims_src_add, 
          dsizes_src_add,
          NULL,
          NULL,
          NULL,
          2);

  src_array = (float*)NclGetArgValue(
          4,
          5,
          &ndims_src_array, 
          dsizes_src_array,
          &missing_src_array,
          &has_missing_src_array,
          NULL,
          2);
/*
 * Check that src_array has a missing value set.
 */
  if(!has_missing_src_array) {
	NhlPError(NhlWARNING,NhlEUNKNOWN,"pop_remap: No missing values are being set.\nDefault missing values will be used.\nBe careful of results.");
	xmsg = 1.e36;
  }
  else {
	xmsg = missing_src_array.floatval;
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

/*
 * Call Fortran popremap.
 */
  NGCALLF(popremap,POPREMAP)(dst_array,map_wts,dst_add,src_add,src_array,
							 &ndst,&nlink,&nw,&nsrc,&xmsg);

  return(NhlNOERROR);
}



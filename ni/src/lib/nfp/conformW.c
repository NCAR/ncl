#include <stdio.h>
#include <string.h>

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

/*   
 * This wrapper takes three arguments, an array of any dimensionality (x), 
 * a 1D array that's the same length of one of the first array's
 * dimensions (dp), and the dimension that should be conformed. 
 * 
 * For example, if dp(klvl):
 *
 *     dpConform = conform (x, dp, 1)    x(ntim,klvl,nlat,mlon)
 *                                        0   1    2    3
 *  then dpConform(ntim,klvl,nlat,mlon)                                       
 */

NhlErrorTypes conform_W( void )
{
/*
 * Input array variables
 */
  void *x;
  NclStackEntry data;
  NclMultiDValData tmp_md = NULL;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;
  int *nconform;
/*
 * Output array variables
 */
  void *conform;
/*
 * various
 */
  int i, j, k, l, ndp, size_before_conform, size_after_conform, sz;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           2);

  data = _NclGetArg(1,3,DONT_CARE);
  switch(data.kind) {
  case NclStk_VAR:
    tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
    break;
  case NclStk_VAL:
    tmp_md = (NclMultiDValData)data.u.data_obj;
    break;
  }

  nconform = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * The ncomform-th dimension of x and dp both must be the same size.
 */
  ndp = tmp_md->multidval.dim_sizes[0];
  if(*nconform < 0 || *nconform > (ndims_x-1)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: The dimension to be conformed must be one of the dimensions of x");
    return(NhlFATAL);
  }
  if( dsizes_x[*nconform] != ndp ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: The length of dp must be the same length as the dimension of x to be conformed");
    return(NhlFATAL);
  }
/*
 * Compute size of other dimensions of x.
 */
  size_before_conform = 1;
  size_after_conform  = 1;
  for(i = 0; i < *nconform; i++) size_before_conform *= dsizes_x[i];
  for(i = *nconform+1; i < ndims_x; i++) size_after_conform *= dsizes_x[i];

/*
 * Allocate space for output array.
 */
  conform = (void*)NclMalloc(size_before_conform*size_after_conform*
                             tmp_md->multidval.totalsize);
  if( conform == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"conform: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  l = 0;
  sz = tmp_md->multidval.type->type_class.size;
  for(i = 0; i < size_before_conform; i++) {
    for(j = 0; j < ndp; j++) {
      for(k = 0; k < size_after_conform; k++) {
        memcpy((void*)((char*)conform + l*sz),
                       (void*)((char*)tmp_md->multidval.val + j*sz),sz);
        l++;
      }
    }
  }

/*
 * Return values.
 */
  if(tmp_md->multidval.missing_value.has_missing) {
    return(NclReturnValue(conform,ndims_x,dsizes_x,
                          &tmp_md->multidval.missing_value.value,
                          tmp_md->multidval.data_type,0));
  }
  else {
    return(NclReturnValue(conform,ndims_x,dsizes_x,NULL,
                          tmp_md->multidval.data_type,0));
  }
}



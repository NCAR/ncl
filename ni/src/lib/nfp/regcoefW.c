#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "NclAtt.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

NhlErrorTypes regcoef_W( void )
{
/*
 * Input array variables
 */
  float *x, *y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y;
  int ndims_extra, has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  float *rcoef, *tval, *xave, *yave;
  int ndims_tval, dsizes_tval[NCL_MAX_DIMENSIONS];
  int ndims_nptxy, dsizes_nptxy[NCL_MAX_DIMENSIONS];
  int ndims_rcoef, *dsizes_rcoef;
  int *nptxy;
/*
 * various
 */
  int i, j, k, l1, l2, l3, total_elem_x, total_elem_y, ier = 0;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           4,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  y = (float*)NclGetArgValue(
           1,
           4,
           &ndims_y, 
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
           2);
/*
 * The x and y coming in can be any dimension, but there are certain rules
 * about having the same dimensions.
 */
  if( ndims_x < 1 || ndims_y < 1 || ndims_x > ndims_y ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The input arrays must be at least 2-dimensional, and y must have as many or more dimensions than x");
    return(NhlFATAL);
  }
/*
 * Get and check number of input points.
 */
  npts = dsizes_x[ndims_x-1];
  if( npts < 2 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The right-most dimension of x must be at least 2");
	return(NhlFATAL);
  }  
/*
 * The x and y missing value attribute must be set.
 */
  if( !has_missing_x) {
	missing_x.floatval =  ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if( !has_missing_y) {
	missing_y.floatval =  ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if(missing_x.floatval != missing_y.floatval) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and y missing values must be the same");
	  return(NhlFATAL);
  }
/*
 * The dimensions of x must be the same as the right-most dimensions of y.
 */
  ndims_extra = ndims_y - ndims_x;
  for(i = 0; i < ndims_x; i++ ) {
	  if( dsizes_x[i] != dsizes_y[ndims_extra + i] ) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The right-most dimensions of y must the same dimensions as x");
		  return(NhlFATAL);
	  }
  }
/*
 * Compute the total number of elements in our x and y arrays.
 * total_elem_x is the total number of elements in the X array minus
 * the last dimension (npts). total_elem_y is the total number of elements
 * in the Y array that go beyond the size of the X array.  For example, if
 * X is dimensioned 2 x 3 x 4, and Y is dimensioned 5 x 2 x 3 x 4, then
 * total_elem_x is 6 (2x3), total_elem_y is 5, and npts is 4.
 */
  ndims_rcoef = max(ndims_y-1,1);
  dsizes_rcoef = (int *)NclMalloc(ndims_rcoef*sizeof(int));
  total_elem_x = 1;
  total_elem_y = 1;
  dsizes_rcoef[0] = 1;
  for(i = 0; i < ndims_x-1; i++) {
	total_elem_x *= dsizes_x[i];
  }
  for(i = 0; i < ndims_extra; i++) {
	total_elem_y *= dsizes_y[i];
  }
  for(i = 0; i < ndims_y-1; i++) {
	dsizes_rcoef[i] = dsizes_y[i];
  }
/*
 * Allocate size for output array (or scalar).
 */
  rcoef = (float *)NclMalloc(total_elem_y*total_elem_x*sizeof(float));
  if( rcoef == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Get output vars.
 */
  tval = (float*)NclGetArgValue(
           2,
           4,
           &ndims_tval, 
           dsizes_tval,
		   NULL,
		   NULL,
           NULL,
           1);
  nptxy = (int*)NclGetArgValue(
           3,
           4,
           &ndims_nptxy, 
           dsizes_nptxy,
		   NULL,
		   NULL,
           NULL,
           1);
/*
 * dimension sizes of tval, nptxy, xave, and yave must be the same as rcoef.
 */
  if( ndims_tval != ndims_rcoef || ndims_nptxy != ndims_rcoef ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The number of dimensions of tval and nptxy must be one less than the number of dimensions of y (or they must both be scalar if y is just a 1-d array)");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_rcoef; i++ ) {
	  if( dsizes_tval[i] != dsizes_rcoef[i] || dsizes_nptxy[i] != dsizes_rcoef[i] ) {
		  NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The dimensions of tval and nptxy must be the same as the left-most dimensions of y");
		  return(NhlFATAL);
	  }
  }
/*
 * Call the f77 version of 'regcoef' with the full argument list.
 */
  l1 = l3 = 0;
  for(i = 1; i <= total_elem_y; i++) {
	l2 = 0;
	for(j = 1; j <= total_elem_x; j++) {
	  NGCALLF(regcoef,REGCOEF)(&x[l2],&y[l1],&npts,&missing_x.floatval,
							   &rcoef[l3],&tval[l3],&nptxy[l3],
							   &xave,&yave,&ier);
	  l1 += npts;
	  l2 += npts;
	  l3++;
	  if (ier == 5) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and/or y array contains all missing values");
		return(NhlFATAL);
	  }
	  if (ier == 6) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"regcoef: The x and/or y array contains less than 3 non-missing values");
		return(NhlFATAL);
	  }
	}
  }
  return(NclReturnValue((void*)rcoef,ndims_rcoef,dsizes_rcoef,NULL,NCL_float,0));
}


NhlErrorTypes regline_W( void )
{
/*
 * Input array variables
 */
  float *x, *y;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
  int ndims_y, dsizes_y[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y;
  int ndims_extra, has_missing_x, has_missing_y, npts;
/*
 * Output array variables
 */
  float *rcoef, *tval, *xave, *yave;
  int *nptxy, ier;
/*
 * Attribute variables
 */
  int att_id, dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  y = (float*)NclGetArgValue(
           1,
           2,
           &ndims_y, 
           dsizes_y,
		   &missing_y,
		   &has_missing_y,
           NULL,
           2);
/*
 * The x and y arrays coming in must be one-dimensional and have
 * the same length.
 */
  if( ndims_x != 1 || ndims_y != 1 || dsizes_x[0] != dsizes_y[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The input arrays must be 1-dimensional and be of the same length");
    return(NhlFATAL);
  }
/*
 * Get and check number of input points.
 */
  npts = dsizes_x[0];
  if( npts < 2 ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The length of x and y must be at least 2");
	return(NhlFATAL);
  }  
/*
 * The x and y missing value attribute must be set.
 */
  if( !has_missing_x) {
	missing_x.floatval =  ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if( !has_missing_y) {
	missing_y.floatval =  ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }
  if(missing_x.floatval != missing_y.floatval) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and y missing values must be the same");
	  return(NhlFATAL);
  }

/*
 * Allocate size for output values.
 */
  rcoef = (float *)NclMalloc(sizeof(float));
  tval  = (float *)NclMalloc(sizeof(float));
  xave  = (float *)NclMalloc(sizeof(float));
  yave  = (float *)NclMalloc(sizeof(float));
  nptxy =   (int *)NclMalloc(sizeof(int));
  if( rcoef == NULL || tval == NULL || xave == NULL || yave == NULL ||
      nptxy == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: Unable to allocate memory for output values");
    return(NhlFATAL);
  }
/*
 * Call the f77 version of 'regline' with the full argument list.
 */
  NGCALLF(regcoef,REGCOEF)(&x[0],&y[0],&npts,&missing_x.floatval,
						   rcoef,tval,nptxy,xave,yave,&ier);
  if (ier == 5) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and/or y array contains all missing values");
	return(NhlFATAL);
  }
  if (ier == 6) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"regline: The x and/or y array contains less than 3 non-missing values");
	return(NhlFATAL);
  }

/*
 * Set up variable to return.
 */
  dsizes[0] = 1;

  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)rcoef,
                        NULL,
                        1,
                        dsizes,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Set up attributes to return.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         tval,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "tval",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         nptxy,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "nptxy",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         xave,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "xave",
             att_md,
             NULL
             );

  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         yave,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "yave",
             att_md,
             NULL
             );

  tmp_var = _NclVarCreate(
                          NULL,
                          NULL,
                          Ncl_Var,
                          0,
                          NULL,
                          return_md,
                          NULL,
                          att_id,
                          NULL,
                          RETURNVAR,
                          NULL,
                          TEMPORARY
                          );
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}


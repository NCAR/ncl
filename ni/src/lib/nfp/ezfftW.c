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

NhlErrorTypes ezfftf_W( void )
{
/*
 * Input array variables
 */
  float *x;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS];
/*
 * Output array variables
 */
  float *cf;
  int dsizes_cf[2];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Attribute variables
 */
  float *xbar;
/*
 * various
 */
  float *work;
  int i, npts, npts2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (float*)NclGetArgValue(
           0,
           1,
           &ndims_x, 
           dsizes_x,
		   NULL,
		   NULL,
           NULL,
           2);
/*
 * Calculate size of output array.
 */
  npts = dsizes_x[0];
  npts2 = (npts/2) + 1;
  dsizes_cf[0] = 2;
  dsizes_cf[1] = npts2;
  cf = (float*)NclMalloc(2*npts2*sizeof(float));
  if ( cf == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for output array" );
	  return(NhlFATAL);
  }
/*
 * Allocate memory for work array
 */
  work = (float*)NclMalloc((3*npts+15)*sizeof(float));
  if ( work == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftf: Cannot allocate memory for work array" );
	  return(NhlFATAL);
  }
/*
 * Call the f77 version of 'ezfftf' with the full argument list.
 */
  NGCALLF(ezffti,EZFFTI)(&npts,work);
  NGCALLF(ezfftf,EZFFTF)(&npts,x,&xbar,&cf[0],&cf[npts2],work);
  free(work);
/*
 * Set up variable to return.
 */
  return_md = _NclCreateVal(
                        NULL,
                        NULL,
                        Ncl_MultiDValData,
                        0,
                        (void*)cf,
                        NULL,
                        2,
                        dsizes_cf,
                        TEMPORARY,
                        NULL,
                        (NclObjClass)nclTypefloatClass
                        );
/*
 * Attributes
 */
  xbar = (float *)NclMalloc(sizeof(float));
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)xbar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "xbar",
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


NhlErrorTypes ezfftb_W( void )
{
/*
 * Input array variables
 */
  float *cf;
  int ndims_cf, dsizes_cf[NCL_MAX_DIMENSIONS];
  float *xbar;
/*
 * Output array variables
 */
  float *x;
  int dsizes_x[1];
/*
 * various
 */
  float *work;
  int i, npts, npts2;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  cf = (float*)NclGetArgValue(
           0,
           2,
           &ndims_cf, 
           dsizes_cf,
		   NULL,
		   NULL,
           NULL,
           2);
  xbar = (float*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Calculate size of output array.
 */
  npts2 = dsizes_cf[1];
  npts = (npts2-1)*2;
  dsizes_x[0] = npts;
  x = (float*)NclMalloc(npts*sizeof(float));
  if ( x == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for output array" );
	  return(NhlFATAL);
  }
/*
 * Allocate memory for work array
 */
  work = (float*)NclMalloc((3*npts+15)*sizeof(float));
  if ( work == NULL ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"ezfftb: Cannot allocate memory for work array" );
	  return(NhlFATAL);
  }
/*
 * Call the f77 version of 'ezfftb' with the full argument list.
 */
  NGCALLF(ezffti,EZFFTI)(&npts,work);
  NGCALLF(ezfftb,EZFFTB)(&npts,x,&xbar,&cf[0],&cf[npts2],work);
  free(work);
  return(NclReturnValue((void*)x,1,dsizes_x,NULL,NCL_float,0));
}


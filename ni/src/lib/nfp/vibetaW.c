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
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <math.h>

#define max(x,y)  ((x) > (y) ? (x) : (y))

NhlErrorTypes vibeta_W( void )
{
/*
 * Input array variables
 */
  float *p, *x, *psfc, *pbot, *ptop;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS], has_missing_p;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_psfc, dsizes_psfc[NCL_MAX_DIMENSIONS];
  NclScalar missing_p, missing_x;
  int *linlog;
/*
 * Output array variables
 */
  float *vint;
  int size_vint, dsizes_vint[NCL_MAX_DIMENSIONS];
/*
 * various
 */
  int i, j, l, ier = 0, nlev;
  float xmsg, plvcrt, xsfc;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  p = (float*)NclGetArgValue(
           0,
           6,
           &ndims_p, 
           dsizes_p,
		   &missing_p,
		   &has_missing_p,
           NULL,
           2);
  x = (float*)NclGetArgValue(
           1,
           6,
           &ndims_x, 
           dsizes_x,
		   &missing_x,
		   &has_missing_x,
           NULL,
           2);
  linlog = (int*)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
		   NULL,
		   NULL,
           NULL,
           2);
  psfc = (float*)NclGetArgValue(
           3,
           6,
           &ndims_psfc, 
           dsizes_psfc,
           NULL,
           NULL,
           NULL,
           2);
  pbot = (float*)NclGetArgValue(
           4,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
  ptop = (float*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           2);
/*
 * Some error checking.
 */
  if(*ptop >= *pbot) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ptop must be less than pbot" );
	return(NhlFATAL);
  }

  if( (ndims_x > 1 && ndims_psfc != ndims_x-1) || (ndims_x == 1 && dsizes_psfc[0] != 1)) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: 'psfc' must have one less dimension than 'x' or else be a constant" );
	return(NhlFATAL);
  }
/*
 * Check for missing values.
 */
  if( has_missing_x ) {
	xmsg = missing_x.floatval;
  }
  else {
	xmsg = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
  }

  for( i = 0; i < ndims_x-1; i++ ) {
	if( dsizes_psfc[i] != dsizes_x[i] ) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The leftmost dimensions of x and psfc must be the same size" );
	  return(NhlFATAL);
	}
  }
  l = 0;
  for( i = 0; i < ndims_psfc-1; i++ ) {
	for( j = 0; j < dsizes_psfc[i]-1; j++ ) {
	  if(psfc[l++] == xmsg) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: psfc must not contain any missing values" );
		return(NhlFATAL);
	  }
	}
  }
  if( dsizes_p[0] != dsizes_x[ndims_x-1] ) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The rightmost dimension of x and p must be the same size" );
	return(NhlFATAL);
  }
  nlev = dsizes_p[0];

  if(p[nlev-1] < *ptop) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: The last element of 'p' must be greater than or equal to ptop" );
	return(NhlFATAL);
  }

  if( nlev < 3 || nlev > 150) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: nlev must be at least 3 and less than 151" );
	return(NhlFATAL);
  }

  plvcrt = p[nlev-1];
  xsfc = x[0];
/*
 * Calculate size of output value.
 */
  size_vint = dsizes_vint[0] = dsizes_psfc[0];
  for( i = 1; i < ndims_psfc; i++ ) {
    dsizes_vint[i] = dsizes_psfc[i];
	size_vint *= dsizes_vint[i];
  }
  vint = (float*)NclMalloc(size_vint*sizeof(float));

/*
 * Call the f77 version of 'vibeta' with the full argument list.
 */
  l = 0;
  for( i = 0; i < size_vint; i++ ) {
	NGCALLF(vibeta,VIBETA)(p,&x[l],&nlev,&xmsg,linlog,&psfc[i],&xsfc,pbot,ptop,&plvcrt,&vint[i],&ier);
	if(ier == -999) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: there must be at least three levels with data above the surface" );
	return(NhlFATAL);
	}
	l += nlev;
  }

  if( !ier ) {
	return(NclReturnValue((void*)vint,ndims_psfc,dsizes_vint,NULL,NCL_float,0));
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"vibeta: ier = %d", ier);
	return(NhlFATAL);
  }
}

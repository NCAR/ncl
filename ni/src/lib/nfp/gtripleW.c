#include <stdio.h>
#include <stdlib.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/

#include "wrapper.h"

extern int NGCALLF(grid2triple,GRID2TRIPLE)(double*,double*,double*,int*,
                                            int*,double*,int*,int*,double*,
                                            int*);

NhlErrorTypes grid2triple_W( void )
{
/*
 * Input array variables
 */
  void *x, *y, *z;
  double *tmp_x, *tmp_y, *tmp_z;
  int dsizes_x[1], dsizes_y[1], dsizes_z[2];
  NclBasicDataTypes type_x, type_y, type_z;
  int has_missing_z;
  NclScalar missing_z, missing_dz, missing_rz;
/*
 * Output array variables
 */
  void *d;
  double *tmp_d;
  int dsizes_d[2];
  NclBasicDataTypes type_d;
/*
 * Various
 */
  int i, mx, ny, ldmax, ldmax2, ldmax3, ld, ld2, ier;
/*
 * Retrieve input array. 
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           2);

  y = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_y,
           NULL,
           NULL,
           &type_y,
           2);

  z = (void*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           2);

  mx  = dsizes_x[0];
  ny  = dsizes_y[0];
  ldmax = mx * ny;
  ldmax2 = 2 * ldmax;
  ldmax3 = 3 * ldmax;
/*
 * Check size of z array.
 */
  if(dsizes_z[0] != ny || dsizes_z[1] != mx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: The last input array must be dimensioned ny x mx, where ny is the length of y, and mx is the length of x");
    return(NhlFATAL);
  }
/*
 * Coerce missing values.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,&missing_rz);
/*
 * Coerce input to double if necessary.
 */
  tmp_x = coerce_input_double(x,type_x,mx,0,NULL,NULL);
  tmp_y = coerce_input_double(y,type_y,ny,0,NULL,NULL);
  tmp_z = coerce_input_double(z,type_z,ldmax,0,NULL,NULL);

  if( tmp_x == NULL || tmp_y == NULL || tmp_z == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to coerce input arrays to double");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array. Since the output array can vary  in
 * size depending on how many missing values there are, we allocate
 * space for a temporary array no matter what.  When we return from the
 * function, we'll create a new array to hold the actual number of
 * non-missing values returned.
 */
  tmp_d = (double*)calloc(ldmax3,sizeof(double));
  if(tmp_d == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
/*
 * Get type of return variable. If any of the input is double, then return
 * a double. Return float otherwise.
 */
  if(type_x == NCL_double || type_y == NCL_double || type_z == NCL_double) {
    type_d = NCL_double;
  }
  else {
    type_d = NCL_float;
  }

  NGCALLF(grid2triple,GRID2TRIPLE)(tmp_x,tmp_y,tmp_z,&mx,&ny,tmp_d,&ldmax,
                                   &ld,&missing_dz.doubleval,&ier);
/*
 * if ld is zero, then this probably means that all of tmp_d is missing,
 * and thus we need to return 3*ldmax missing values.
 */
  if(ld == 0) ld = ldmax;

  dsizes_d[0] = 3;
  dsizes_d[1] = ld;
  if(type_d == NCL_double) {
    d = (void*)calloc(3*ld,sizeof(double));
  }
  else {
    d = (void*)calloc(3*ld,sizeof(float));
  }
  if(d == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }

  if(!ier) {
    ld2 = 2*ld;
/*
 * ld contains the number of non-missing values. It should be <= ldmax.
 *
 * The first ld elements of tmp_d will be the non-missing ones.
 */
    if(type_d == NCL_float) {
      for( i = 0; i < ld; i++ ) {
        ((float*)d)[i]     = (float)tmp_d[i];
        ((float*)d)[i+ld]  = (float)tmp_d[i+ldmax];
        ((float*)d)[i+ld2] = (float)tmp_d[i+ldmax2];
      }
    }
    else {
      for( i = 0; i < ld; i++ ) {
        ((double*)d)[i]     = tmp_d[i];
        ((double*)d)[i+ld]  = tmp_d[i+ldmax];
        ((double*)d)[i+ld2] = tmp_d[i+ldmax2];
      }
    }
  }
  else {
    if(ier == -10) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"grid2triple: all z input values are missing\n");
      set_subset_output_missing(d,0,type_d,ldmax3,missing_dz.doubleval);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_y != NCL_double) NclFree(tmp_y);
  if(type_z != NCL_double) NclFree(tmp_z);
  NclFree(tmp_d);
/*
 * Return.
 */
  if(ier) {
    if(type_d == NCL_double) {
      return(NclReturnValue(d,2,dsizes_d,&missing_dz,type_d,0));
    }
    else {
      return(NclReturnValue(d,2,dsizes_d,&missing_rz,type_d,0));
    }
  }
  else {
    return(NclReturnValue(d,2,dsizes_d,NULL,type_d,0));
  }
}

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
  double *tmp_d, zmsg;
  int dsizes_d[2];
  NclBasicDataTypes type_d;
/*
 * Various
 */
  int i, j, mx, ny, ldmax, ld, ier;
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
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dz,
                 &missing_rz);
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
 * size depending on how many missing values there are, we allocated
 * space for a temporary array no matter what.
 */
  tmp_d = (double*)calloc(3*ldmax,sizeof(double));
  if(tmp_d == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"grid2triple: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
/*
 * Get missing value.
 */
  if(type_z == NCL_double) {
    type_d = NCL_double;
    zmsg = missing_dz.doubleval;
  }
  else {
    type_d = NCL_float;
    zmsg = (double)missing_rz.floatval;
  }

  NGCALLF(grid2triple,GRID2TRIPLE)(tmp_x,tmp_y,tmp_z,&mx,&ny,tmp_d,&ldmax,
                   &ld,&zmsg,&ier);
  if(!ier) {
/*
 * ld contains the number of non-missing values. It should be <= ldmax.
 */
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
/*
 * Only return values that aren't missing.
 */
    j = 0;
    if(type_d == NCL_float) {
      for( i = 0; i < ldmax; i++ ) {
    if(tmp_d[i] != zmsg) {
      ((float*)d)[j] = tmp_d[i];
      j++;
    }
      }
    }
    else {
      for( i = 0; i < ldmax; i++ ) {
    if(tmp_d[i] != zmsg) {
      ((double*)d)[j] = tmp_d[i];
      j++;
    }
      }
    }
  }
  else {
    if(ier == -10) {
      ld = ldmax;
      NhlPError(NhlWARNING,NhlEUNKNOWN,"grid2triple: all values missing\n");
      set_subset_output_missing(d,0,type_d,ldmax,zmsg);
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_y);
  NclFree(tmp_z);
  NclFree(tmp_d);
/*
 * Return.
 */
  return(NclReturnValue(d,2,dsizes_d,NULL,type_d,0));
}


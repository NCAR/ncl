#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(kerdeni,KERDENI)(double *, int *, double *, double *, int *, double *, double *);

NhlErrorTypes dim_kde_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x = NULL;
  int       ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_flt_x, missing_dbl_x;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  void *bin;
  double *tmp_bin = NULL;
  ng_size_t dsizes_bin[1];
  NclBasicDataTypes type_bin;

/*
 * Return variable
 */
  void *kde;
  double *tmp_kde = NULL;
  int       ndims_kde;
  ng_size_t *dsizes_kde;
  int has_missing_kde;
  NclScalar missing_kde, missing_flt_kde, missing_dbl_kde;
  NclBasicDataTypes type_kde;


/*
 * Various
 */
  int n, m;
  int index_x, index_kde;
  double h;
  int i, ndims_leftmost, size_leftmost, size_output, ret;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x,
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,
                 &missing_dbl_x,&missing_flt_x);

  n = 1;
  for(i = 0; i < ndims_x; i++)
    n *= dsizes_x[i];
/*
 * Get argument # 1
 */
  bin = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_bin,
           NULL,
           NULL,
           &type_bin,
           DONT_CARE);
  m = dsizes_bin[0];

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = 1;
/* assume size_leftmost and ndims_leftmost =1 for now
  ndims_leftmost = ndims_x-1;
  for(i = 0; i < ndims_leftmost; i++) {
    size_leftmost *= dsizes_x[i];
  }
*/

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_x.
 */
  if(type_x != NCL_double) {
    tmp_x = (double *)calloc(n,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
/*
 * The output type defaults to float, unless this input array is double.
 */
    type_kde = NCL_float;
  }
  else {
    type_kde = NCL_double;
  }
/*
 * Allocate space for tmp_bin.
 */
  tmp_bin = coerce_input_double(bin,type_bin,m,0,NULL,NULL);
  if(tmp_bin == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftmost * m;

/* 
 * Allocate space for output array.
 */
  if(type_kde != NCL_double) {
    kde = (void *)calloc(size_output, sizeof(float));
    tmp_kde = (double *)calloc(m,sizeof(double));
    if(tmp_kde == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    kde = (void *)calloc(size_output, sizeof(double));
  }
  if(kde == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_x) {
    if(type_kde == NCL_double)
        missing_kde = missing_dbl_x;
    else
        missing_kde = missing_flt_x;
    missing_dbl_kde = missing_dbl_x;
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_kde = 1;
  dsizes_kde = (ng_size_t*)calloc(ndims_kde,sizeof(ng_size_t));  
/* assume 1 dimension for now
  ndims_kde = ndims_leftmost + 1;
  dsizes_kde = (ng_size_t*)calloc(ndims_kde,sizeof(ng_size_t));  
  if( dsizes_kde == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_kde-1; i++) dsizes_kde[i] = dsizes_x[i];
*/
  dsizes_kde[ndims_kde-1] = m;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = index_kde = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
    if(type_x != NCL_double) {
      coerce_subset_input_double(x,tmp_x,index_x,type_x,n,0,NULL,NULL);
    }
    else {
      tmp_x = &((double*)x)[index_x];
    }


/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_kde == NCL_double) tmp_kde = &((double*)kde)[index_kde];


/*
 * Call the Fortran routine.
 */
    NGCALLF(kerdeni,KERDENI)(tmp_x, &n, &missing_dbl_x.doubleval, tmp_bin, &m, tmp_kde, &h);

/*
 * Coerce output back to float if necessary.
 */
    if(type_kde == NCL_float) {
      coerce_output_float_only(kde,tmp_kde,m,index_kde);
    }
    index_x += n;
    index_kde += m;
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_bin != NCL_double) NclFree(tmp_bin);
  if(type_kde != NCL_double) NclFree(tmp_kde);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(kde,ndims_kde,dsizes_kde,&missing_kde,type_kde,0);
  NclFree(dsizes_kde);
  return(ret);
}

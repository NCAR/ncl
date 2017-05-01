#include <stdio.h>
#include "wrapper.h"

#define DEBUG

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
  double *tmp_input = NULL;
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
 * Argument # 2
 */
  int *dims;
  int *tmp_dims = NULL;
  int ndims_dims;
  ng_size_t dsizes_dims[1];
  NclBasicDataTypes type_dims;

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
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md;

/*
 * Various
 */
  int size_total, m;
  int index_x, index_kde;
  double h;
  int i, j, k, size_output, ret;
  int ndims_leftover, size_leftover;
  ng_size_t *dsizes_leftover;
  int ndims_leftmost, size_leftmost;
  ng_size_t *dsizes_leftmost;
  int ndims_rightmost, size_rightmost;
  ng_size_t *dsizes_rightmost;
  int ndims_middle, size_middle;
  ng_size_t *dsizes_middle;
  int offset_to_major_chunk, offset_to_minor_chunk;

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
           3,
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

  size_total = 1;
  for(i = 0; i < ndims_x; i++)
    size_total *= dsizes_x[i];
/*
 * Get argument # 1
 */
  bin = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_bin,
           NULL,
           NULL,
           &type_bin,
           DONT_CARE);
  m = dsizes_bin[0];

/*
 * Get argument # 2
 */
  dims = (int*)NclGetArgValue(
           2,
           3,
           NULL,
           dsizes_dims,
           NULL,
           NULL,
           &type_dims,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions.
 */
  int dim;
  int previous_dim = -1;
  for(i = 0; i < dsizes_dims[0]; i++) {
    dim = ((int *)dims)[i];
    if((dim != previous_dim + 1) && (previous_dim != -1))
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: dims must be consecutive and monotonically increasing");
    previous_dim = dim;
  }

  ndims_dims = dsizes_dims[0];
  int first_dim = ((int *)dims)[0];
  int last_dim;
  printf("ndims_dims: %d\n", ndims_dims);
  if(ndims_dims == 1) 
    last_dim = first_dim;
  else
    last_dim = ((int *)dims)[ndims_dims - 1];

  if(first_dim < 0 || first_dim >= ndims_x)
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: invalid dims");
  else {
    size_leftmost = 1;
    ndims_leftmost = 0;
    for(i = 0; i < first_dim; i++) {
      size_leftmost *= dsizes_x[i];
      ndims_leftmost++;
    }
  }

  if(last_dim < 0 || last_dim >= ndims_x)
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: invalid dims");
  else {
    size_rightmost = 1;
    ndims_rightmost = 0;
    for(i = last_dim + 1; i < ndims_x; i++) {
      size_rightmost *= dsizes_x[i];
      ndims_rightmost++;
    }
  }

  size_leftover = size_leftmost * size_rightmost;
  ndims_leftover = ndims_leftmost + ndims_rightmost;
  size_middle = size_total / size_leftover;

  printf("size_leftmost: %d\n", size_leftmost);
  printf("ndims_leftmost: %d\n", ndims_leftmost);
  printf("size_rightmost: %d\n", size_rightmost);
  printf("ndims_rightmost: %d\n", ndims_rightmost);
  printf("size_leftover: %d\n", size_leftover);
  printf("ndims_leftover: %d\n", ndims_leftover);

  printf("first_dim: %d\n", first_dim);
  printf("last_dim: %d\n", last_dim);

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
    tmp_x = (double *)calloc(size_middle,sizeof(double));
    if(tmp_x == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
/*
 * Allocate space for tmp_input.
 */
/*
  tmp_input = (double *)calloc(size_leftover,sizeof(double));
  if(tmp_input == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_input: Unable to allocate memory for temporary input array");
    return(NhlFATAL);
  }
*/
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
for(i = 0; i < m; i++) printf("%f\n", ((float *)bin) + i);
for(i = 0; i < m; i++) printf("%f\n", tmp_bin + i);

/*
 * Calculate size of output array.
 */
  size_output = size_leftover * m;

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
  ndims_kde = ndims_leftover + 1;
  dsizes_kde = (ng_size_t*)calloc(ndims_kde,sizeof(ng_size_t));  
  if( dsizes_kde == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_kde: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  int index_dims = 0;
  index_kde = 0;
  for(index_x = 0; index_x < ndims_x; index_x++) {
    /*if(index_x == ((int *)dims)[index_dims])
      index_dims++;*/
    if(index_x == ((int *)dims)[index_x - index_kde])
      continue;
    else
      dsizes_kde[index_kde++] = dsizes_x[index_x];
  }
  dsizes_kde[ndims_kde-1] = m;


  for(i = 0; i < ndims_kde; i++) printf("%d\n", dsizes_kde[i]);


/*
 * Loop across leftover dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = index_kde = 0;

#ifdef DEBUG
/*
      printf("size_leftover: %d\n", size_leftover);
*/
#endif

  for(i = 0; i < size_leftmost; i++) {
    offset_to_major_chunk = i * size_rightmost * size_middle;
    for(j = 0; j < size_rightmost; j++) {
      index_kde = m * (i + j);
      offset_to_minor_chunk = j * size_middle;

/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
      for(k = 0; k < size_middle; k++) {
        // read size_middle/mult(middle_dims) at  j * size_rightmost
        if(type_x != NCL_double) {
          /*coerce_subset_input_double(x,tmp_x,offset_to_major_chunk + offset_to_minor_chunk,type_x,size_leftover,0,NULL,NULL);*/
          /*coerce_subset_input_double(x,tmp_x + k,offset_to_major_chunk + offset_to_minor_chunk + k * size_rightmost,type_x,1,0,NULL,NULL);*/
          coerce_subset_input_double(x,tmp_x + k,offset_to_major_chunk + offset_to_minor_chunk + k,type_x,1,0,NULL,NULL);
/*
          printf("maj min: %d %d\n", offset_to_major_chunk, offset_to_minor_chunk);
          printf("x[%d]: %f\n", offset_to_major_chunk + offset_to_minor_chunk + k, *((float *)x + offset_to_major_chunk + offset_to_minor_chunk + k));
          printf("tmp_x[%d]: %f\n", k, *(tmp_x + k));
          printf("combined_offset: %d\n", k + offset_to_major_chunk + offset_to_minor_chunk);
*/
        }
        else {
          /*tmp_x = memcpy(tmp_x + k * size_middle, x + offset_to_major_chunk + offset_to_minor_chunk, size_leftover);*/
          tmp_x = memcpy(tmp_x + k * size_middle, x + offset_to_major_chunk + offset_to_minor_chunk, sizeof(double));
        }
      }

/*
      if(type_x != NCL_double) {
        coerce_subset_input_double(x,tmp_x,index_x,type_x,n,0,NULL,NULL);
      }
      else {
        tmp_x = &((double*)x)[index_x];
      }
*/ 
/*
 * Point temporary output array to void output array if appropriate.
 */
      if(type_kde == NCL_double) tmp_kde = &((double*)kde)[index_kde];
    
    
/*
 * Call the Fortran routine.
 */
      NGCALLF(kerdeni,KERDENI)(tmp_x, &size_middle, &missing_dbl_x.doubleval, tmp_bin, &m, tmp_kde, &h);
int a;
for(a = 0; a < size_middle; a ++) printf("%f\n", tmp_x[a]);
for(a = 0; a < m; a ++) printf("%f\n", tmp_kde[a]);
for(a = 0; a < m; a ++) printf("%f\n", tmp_bin[a]);

/*
printf("size_middle: %d\n", size_middle);
printf("m: %d\n", m);
printf("h: %d\n", h);
*/
/*
 * Coerce output back to float if necessary.
 */
      if(type_kde == NCL_float) {
        coerce_output_float_only(kde,tmp_kde,m,index_kde);
      }
/*      index_x += n; // don't need this, will index manually */
/*      index_kde += m * size_leftmost; */
/*      index_kde += m * (size_leftmost + size_rightmost); */
/*      index_kde = m * (i * size_leftmost + j * size_rightmost); */
      index_kde = m * (i + j);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_bin != NCL_double) NclFree(tmp_bin);
  if(type_kde != NCL_double) NclFree(tmp_kde);

/*
 * Set binwidth attribute
 */ 
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  ng_size_t dsizes[1];
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)&h,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );

  _NclAddAtt(
             att_id,
             "binwidth",
             att_md,
             NULL
             );


/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(kde,ndims_kde,dsizes_kde,&missing_kde,type_kde,0);
  NclFree(dsizes_kde);
  return(ret);
}

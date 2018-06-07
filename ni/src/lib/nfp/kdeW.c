#include <stdio.h>
#include "wrapper.h"

#define DEBUG

extern void NGCALLF(kerdeni,KERDENI)(double *, int *, double *, double *, int *, double *, double *);

NhlErrorTypes kde_n_W( void )
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
  NclScalar missing_x, missing_dbl_x;
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
  NclBasicDataTypes type_kde;

/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int ndims_h;
  ng_size_t *dsizes_h;

/*
 * Various
 */
  int size_total, m;
  int index_x, index_kde;
  void *h;
  double tmp_h;
  int i, j, size_output, ret;
  int ndims_leftover, size_leftover;
/*  ng_size_t *dsizes_leftover; */
  int ndims_leftmost, size_leftmost;
  int ndims_rightmost, size_rightmost;
  int size_middle;

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
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dbl_x,NULL);

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
 * Determine dims
 */
  ndims_dims = dsizes_dims[0];
  int first_dim = ((int *)dims)[0];
  int last_dim;
  if(ndims_dims == 1)
    last_dim = first_dim;
  else
    last_dim = ((int *)dims)[ndims_dims - 1];
  if(first_dim < 0 || first_dim >= ndims_x || last_dim < 0 || last_dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: invalid dims",KDE_NAME);
    return(NhlFATAL);
  }

/*
 * Calculate size of leftmost dimensions.
 */
  int dim;
  int previous_dim = -1;
  for(i = 0; i < dsizes_dims[0]; i++) {
    dim = ((int *)dims)[i];
    if((dim != previous_dim + 1) && (previous_dim != -1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: dims must be consecutive and monotonically increasing",KDE_NAME);
      return(NhlFATAL);
    }
    previous_dim = dim;
  }

  if (ndims_x == ndims_dims) {
    ndims_h = 1;
  } else {
    ndims_h = ndims_x - ndims_dims;
  }
  /* make this float eventually */
  dsizes_h = (ng_size_t *)calloc(ndims_h,sizeof(ng_size_t));
  if(dsizes_h == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for attribute dsizes array",KDE_NAME);
    return(NhlFATAL);
  }
  /* initialize to 1 in case h is a scalar */
  dsizes_h[0] = 1;

  size_leftmost = 1;
  ndims_leftmost = 0;
  for(i = 0; i < first_dim; i++) {
    size_leftmost *= dsizes_x[i];
    dsizes_h[i] = dsizes_x[i];
    ndims_leftmost++;
  }

  size_rightmost = 1;
  ndims_rightmost = 0;
  for(i = last_dim + 1; i < ndims_x; i++) {
    size_rightmost *= dsizes_x[i];
    dsizes_h[i - ndims_dims] = dsizes_x[i];
    ndims_rightmost++;
  }

  size_leftover = size_leftmost * size_rightmost;
  ndims_leftover = ndims_leftmost + ndims_rightmost;
  size_middle = size_total / size_leftover;

/*
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for h.
 */
  h = (double *)calloc(size_leftover,sizeof(double));
  if(h == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for attribute array",KDE_NAME);
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(size_middle,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for coercing input array to double",KDE_NAME);
    return(NhlFATAL);
  }
/*
 * The output type defaults to float, unless this input array is double.
 */
  if(type_x != NCL_double) {
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for coercing input array to double",KDE_NAME);
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  size_output = size_leftover * m;

/*
 * Allocate space for output array.
 */
  if(type_kde != NCL_double) {
    h = (void *)calloc(size_leftover,sizeof(float));
    kde = (void *)calloc(size_output, sizeof(float));
    tmp_kde = (double *)calloc(m,sizeof(double));
    if(tmp_kde == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for temporary output array",KDE_NAME);
      return(NhlFATAL);
    }
  }
  else {
    h = (void *)calloc(size_leftover,sizeof(double));
    kde = (void *)calloc(size_output, sizeof(double));
  }
  if(kde == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for output array",KDE_NAME);
    return(NhlFATAL);
  }
  if(h == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for output attribute",KDE_NAME);
    return(NhlFATAL);
  }

/*
 * Allocate space for output dimension sizes and set them.
 */
  ndims_kde = ndims_leftover + 1;
  dsizes_kde = (ng_size_t*)calloc(ndims_kde,sizeof(ng_size_t));
  if( dsizes_kde == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Unable to allocate memory for holding dimension sizes",KDE_NAME);
    return(NhlFATAL);
  }
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

/*
 * Loop across leftover dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_x = index_kde = 0;

int index_h = 0;
int index_nrx;

  for(i = 0; i < size_leftmost; i++) {
    index_nrx = i * size_rightmost * size_middle;
    for(j = 0; j < size_rightmost; j++) {
      index_x = index_nrx + j;
      index_kde = (i + j) * m;

/*
 * Coerce subsection of x (tmp_x) to double if necessary.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,size_rightmost,type_x,size_middle,0,NULL,NULL);

/*
 * Point temporary output array to void output array if appropriate.
 */
      if(type_kde == NCL_double) tmp_kde = &((double*)kde)[index_kde];

/*
 * Call the Fortran routine.
 */
      NGCALLF(kerdeni,KERDENI)(tmp_x, &size_middle, &missing_dbl_x.doubleval, tmp_bin, &m, tmp_kde, &tmp_h);

/*
 * Coerce output back to float if necessary.
 */
      coerce_output_float_or_double(kde,tmp_kde,type_kde,m,index_kde);
      coerce_output_float_or_double(h,&tmp_h,type_kde,1,index_h++);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_x != NCL_double) NclFree(tmp_x);
  if(type_bin != NCL_double) NclFree(tmp_bin);
  if(type_kde != NCL_double) NclFree(tmp_kde);

/*
 * Set up return structure and set bandwidth attribute
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
  if(type_kde == NCL_float) {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              kde,
                              NULL,
                              ndims_kde,
                              dsizes_kde,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           h,
                           NULL,
                           ndims_h,
                           dsizes_h,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  } else {
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              kde,
                              NULL,
                              ndims_kde,
                              dsizes_kde,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           h,
                           NULL,
                           ndims_h,
                           dsizes_h,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }

  _NclAddAtt(
             att_id,
             "bandwidth",
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

  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);


/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(kde,ndims_kde,dsizes_kde,NULL,type_kde,0);
  NclFree(dsizes_kde);
  return(ret);
}

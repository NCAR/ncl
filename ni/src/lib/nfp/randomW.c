#include <stdio.h>
#include "wrapper.h"

extern void   NGCALLF(setall,SETALL)(int*,int*);
extern double NGCALLF(dgenchi,DGENCHI)(double*);
extern double NGCALLF(dgengam,DGENGAM)(double*,double*);
extern double NGCALLF(dgennor,DGENNOR)(double*,double*);
extern double NGCALLF(dgenunf,DGENUNF)(double*,double*);

NhlErrorTypes random_setallseed_W( void )
{
/*
 * Input array variables
 */
  int *seed1, *seed2;

/*
 * Retrieve arguments.
 */
  seed1 = (int*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  seed2 = (int*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Call Fortran routine to set the seed.
 */
  NGCALLF(setall,SETALL)(seed1,seed2);

  return(NhlNOERROR);

}

NhlErrorTypes random_chi_W( void )
{
/*
 * Input array variables
 */
  void *df;
  double *tmp_df;
  void *N;
  ng_size_t dsizes_N[1];
  NclBasicDataTypes type_df, type_N;
/*
 * Output array variables
 */
  void *chi;
  double *tmp_chi = NULL;
  ng_size_t *dsizes_chi;
  NclBasicDataTypes type_chi;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t i, size_input;
/*
 * Retrieve argument.
 */
  df = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_df,
          DONT_CARE);

  N = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          dsizes_N,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimensions and compute the total size of the input array.
 */
  dsizes_chi = get_dimensions(N,dsizes_N[0],type_N,"random_chi");
  if(dsizes_chi == NULL) 
    return(NhlFATAL);

  size_input = 1;
  for (i = 0; i < dsizes_N[0]; i++) size_input *= dsizes_chi[i];

/*
 * Coerce input to double if necessary. 
 */
  tmp_df = coerce_input_double(df,type_df,1,0,NULL,NULL);

/*
 * Create space for output array.
 */
  if(type_df != NCL_double) {
    type_chi = NCL_float;
    chi      = (void*)calloc(size_input,sizeof(float));
    tmp_chi  = (double*)calloc(1,sizeof(double));
  }
  else {
    type_chi = NCL_double;
    chi      = (void*)calloc(size_input,sizeof(double));
  }
  if(chi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"random_chi: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop through each element and call the Fortran version of this
 * routine.
 */
  for( i = 0; i < size_input; i++ ) {
    if(type_chi == NCL_double) tmp_chi = &((double*)chi)[i];

/*    
 * Call Fortran routine.
 */ 
    *tmp_chi = NGCALLF(dgenchi,DGENCHI)(tmp_df);
/*
 * Coerce output to float if necessary.
 */
    if(type_chi != NCL_double) coerce_output_float_only(chi,tmp_chi,1,i);
  }
/*
 * Free memory.
 */
  if(type_df  != NCL_double) NclFree(tmp_df);
  if(type_chi != NCL_double) NclFree(tmp_chi);
/*
 * Return.
 */
  ret = NclReturnValue(chi,dsizes_N[0],dsizes_chi,NULL,type_chi,0);
  NclFree(dsizes_chi);
  return ret;
}

NhlErrorTypes random_gamma_W( void )
{
/*
 * Input array variables
 */
  void *locp, *shape;
  double *tmp_locp, *tmp_shape;
  void *N;
  ng_size_t dsizes_N[1];
  NclBasicDataTypes type_locp, type_shape, type_N;
/*
 * Output array variables
 */
  void *gamma;
  double *tmp_gamma = NULL;
  ng_size_t *dsizes_gamma;
  NclBasicDataTypes type_gamma;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t i, size_input;
/*
 * Retrieve argument.
 */
  locp = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_locp,
          DONT_CARE);

  shape = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_shape,
          DONT_CARE);

  N = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_N,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimensions and compute the total size of the input array.
 */
  dsizes_gamma = get_dimensions(N,dsizes_N[0],type_N,"random_gamma");
  if(dsizes_gamma == NULL) 
    return(NhlFATAL);

  size_input = 1;
  for (i = 0; i < dsizes_N[0]; i++) size_input *= dsizes_gamma[i];

/*
 * Coerce input to double if necessary. 
 */
  tmp_locp  = coerce_input_double(locp,type_locp,1,0,NULL,NULL);
  tmp_shape = coerce_input_double(shape,type_shape,1,0,NULL,NULL);

/*
 * Create space for output array.
 */
  if(type_locp != NCL_double && type_shape != NCL_double) {
    type_gamma = NCL_float;
    gamma      = (void*)calloc(size_input,sizeof(float));
    tmp_gamma  = (double*)calloc(1,sizeof(double));
  }
  else {
    type_gamma = NCL_double;
    gamma      = (void*)calloc(size_input,sizeof(double));
  }
  if(gamma == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"random_gamma: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop through each element and call the Fortran version of this
 * routine.
 */
  for( i = 0; i < size_input; i++ ) {
    if(type_gamma == NCL_double) tmp_gamma = &((double*)gamma)[i];

/*    
 * Call Fortran routine.
 */ 
    *tmp_gamma = NGCALLF(dgengam,DGENGAM)(tmp_locp,tmp_shape);

    if(type_gamma != NCL_double) {
      coerce_output_float_only(gamma,tmp_gamma,1,i);
    }
  }
/*
 * Free memory.
 */
  if(type_locp  != NCL_double) NclFree(tmp_locp);
  if(type_shape != NCL_double) NclFree(tmp_shape);
  if(type_gamma != NCL_double) NclFree(tmp_gamma);
/*
 * Return.
 */
  ret = NclReturnValue(gamma,dsizes_N[0],dsizes_gamma,NULL,type_gamma,0);
  NclFree(dsizes_gamma);
  return ret;
}


NhlErrorTypes random_normal_W( void )
{
/*
 * Input array variables
 */
  void *av, *sd;
  double *tmp_av, *tmp_sd;
  void *N;
  ng_size_t dsizes_N[1];
  NclBasicDataTypes type_av, type_sd, type_N;
/*
 * Output array variables
 */
  void *normal;
  double *tmp_normal = NULL;
  ng_size_t *dsizes_normal;
  NclBasicDataTypes type_normal;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t i, size_input;
/*
 * Retrieve argument.
 */
  av = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_av,
          DONT_CARE);

  sd = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_sd,
          DONT_CARE);

  N = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_N,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimensions and compute the total size of the input array.
 */
  dsizes_normal = get_dimensions(N,dsizes_N[0],type_N,"random_normal");
  if(dsizes_normal == NULL) 
    return(NhlFATAL);

  size_input = 1;
  for (i = 0; i < dsizes_N[0]; i++) size_input *= dsizes_normal[i];

/*
 * Coerce input to double if necessary. 
 */
  tmp_av = coerce_input_double(av,type_av,1,0,NULL,NULL);
  tmp_sd = coerce_input_double(sd,type_sd,1,0,NULL,NULL);

/*
 * Create space for output array.
 */
  if(type_av != NCL_double && type_sd != NCL_double) {
    type_normal = NCL_float;
    normal      = (void*)calloc(size_input,sizeof(float));
    tmp_normal  = (double*)calloc(1,sizeof(double));
  }
  else {
    type_normal = NCL_double;
    normal      = (void*)calloc(size_input,sizeof(double));
  }
  if(normal == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"random_normal: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop through each element and call the Fortran version of this
 * routine.
 */
  for( i = 0; i < size_input; i++ ) {
    if(type_normal == NCL_double) tmp_normal = &((double*)normal)[i];
/*    
 * Call Fortran routine.
 */ 
    *tmp_normal = NGCALLF(dgennor,DGENNOR)(tmp_av,tmp_sd);
/*
 * Coerce output to float if necessary.
 */
    if(type_normal != NCL_double) {
      coerce_output_float_only(normal,tmp_normal,1,i);
    }
  }
/*
 * Free memory.
 */
  if(type_av     != NCL_double) NclFree(tmp_av);
  if(type_sd     != NCL_double) NclFree(tmp_sd);
  if(type_normal != NCL_double) NclFree(tmp_normal);
/*
 * Return.
 */
  ret = NclReturnValue(normal,dsizes_N[0],dsizes_normal,NULL,type_normal,0);
  NclFree(dsizes_normal);
  return ret;
}


NhlErrorTypes random_uniform_W( void )
{
/*
 * Input array variables
 */
  void *low, *high;
  double *tmp_low, *tmp_high;
  void *N;
  ng_size_t dsizes_N[1];
  NclBasicDataTypes type_low, type_high, type_N;
/*
 * Output array variables
 */
  void *uniform;
  double *tmp_uniform = NULL;
  ng_size_t *dsizes_uniform;
  NclBasicDataTypes type_uniform;
/*
 * Declare various variables for random purposes.
 */
  int ret;
  ng_size_t i, size_input;
/*
 * Retrieve argument.
 */
  low = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_low,
          DONT_CARE);

  high = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_high,
          DONT_CARE);

  N = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_N,
          NULL,
          NULL,
          &type_N,
          DONT_CARE);

/*
 * Check the input dimensions and compute the total size of the input array.
 */
  dsizes_uniform = get_dimensions(N,dsizes_N[0],type_N,"random_uniform");
  if(dsizes_uniform == NULL) 
    return(NhlFATAL);

  size_input = 1;
  for (i = 0; i < dsizes_N[0]; i++) size_input *= dsizes_uniform[i];

/*
 * Coerce input to double if necessary. 
 */
  tmp_low  = coerce_input_double(low,type_low,1,0,NULL,NULL);
  tmp_high = coerce_input_double(high,type_high,1,0,NULL,NULL);

/*
 * Create space for output array.
 */
  if(type_low != NCL_double && type_high != NCL_double) {
    type_uniform = NCL_float;
    uniform      = (void*)calloc(size_input,sizeof(float));
    tmp_uniform  = (double*)calloc(1,sizeof(double));
  }
  else {
    type_uniform = NCL_double;
    uniform      = (void*)calloc(size_input,sizeof(double));
  }
  if(uniform == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"random_uniform: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop through each element and call the Fortran version of this
 * routine.
 */
  for( i = 0; i < size_input; i++ ) {
    if(type_uniform == NCL_double) tmp_uniform = &((double*)uniform)[i];
/*    
 * Call Fortran routine.
 */ 
    *tmp_uniform = NGCALLF(dgenunf,DGENUNF)(tmp_low,tmp_high);
/*
 * Coerce output to float if necessary.
 */
    if(type_uniform != NCL_double) {
      coerce_output_float_only(uniform,tmp_uniform,1,i);
    }
  }
/*
 * Free memory.
 */
  if(type_low     != NCL_double) NclFree(tmp_low);
  if(type_high    != NCL_double) NclFree(tmp_high);
  if(type_uniform != NCL_double) NclFree(tmp_uniform);
/*
 * Return.
 */
  ret = NclReturnValue(uniform,dsizes_N[0],dsizes_uniform,NULL,type_uniform,0);
  NclFree(dsizes_uniform);
  return ret;
}


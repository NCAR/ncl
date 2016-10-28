/* THIS WRAPPER IS NOT DONE! I'M CHECKING IT IN BEFORE THE SVN TO GIT TRANSITION */

#include <stdio.h>
#include "wrapper.h"
#include "eemd.h"

extern size_t emd_num_imfs(size_t);
extern libeemd_error_code ceemdan(double const* restrict, size_t *, double * restrict, 
				  size_t *, unsigned int *, double, unsigned int,
				  unsigned int, unsigned long int);

NhlErrorTypes ceemdan_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *input;
  double *tmp_input = NULL;
  int       ndims_input;
  ng_size_t dsizes_input[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_input;

/*
 * Argument # 1
 */
  void *ensemble_size;
  double *tmp_ensemble_size = NULL;
  NclBasicDataTypes type_ensemble_size;

/*
 * Argument # 2
 */

  void *noise_strength;
  double *tmp_noise_strength = NULL;
  NclBasicDataTypes type_noise_strength;

/*
 * Argument # 3
 */
  void *S_number;
  unsigned int *tmp_S_number = NULL;
  NclBasicDataTypes type_S_number;

/*
 * Argument # 4
 */
  void *num_siftings;
  unsigned int *tmp_num_siftings = NULL;
  NclBasicDataTypes type_num_siftings;

/*
 * Argument # 5
 */
  void *rng_seed;
  unsigned long int *tmp_rng_seed = NULL;
  NclBasicDataTypes type_rng_seed;

/*
 * Return variable
 */
  void *output;
  double *tmp_output = NULL;
  int       ndims_output;
  ng_size_t dsizes_output;
  NclBasicDataTypes type_output;

/*
 * Various
 */
  ng_size_t nlon, nlat, ntim, ntnltnln, nimntnltnln;
  ng_size_t index_input;
  ng_size_t i,  size_output;
  size_t num_imfs;
  libeemd_error_code error_code;
  int ret;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  input = (void*)NclGetArgValue(
           0,
           6,
           &ndims_input,
           dsizes_input,
           NULL,
           NULL,
           &type_input,
           DONT_CARE);

/*
 * We've prototyped 'input' to be 3D for now, but leave it somewhat generic for
 * now in case we decide to add leftmost dimensions later.
 */
  if(ndims_input != 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: The input array must be three-dimensional")
    return(NhlFATAL);
  }
  ntim = dsizes_input[ndims_input-3];
  nlat = dsizes_input[ndims_input-2];
  nlon = dsizes_input[ndims_input-1];
  ntnltnln = ntim * nlat * nlon;
  num_imfs = emd_num_imfs((size_t)ntnltnlon);
  if(num_imfs == 0 || num_imfs == 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: the input spatial domain must contain at least 3 values.")
    return(NhlFATAL);
  }
  nimntnltnln = num_imfs * ntnltnln;
/*
 * Get argument # 1
 */
  ensemble_size = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_ensemble_size,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_ensemble_size == NCL_float || type_ensemble_size == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: ensemble_size must be an integral type (int, uint, long, etc)");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  noise_strength = (void*)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_noise_strength,
           DONT_CARE);
/*
 * Get argument # 3
 */
  S_number = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_S_number,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_S_number == NCL_float || type_S_number == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: S_number must be an integral type (int, uint, long, etc)");
    return(NhlFATAL);
  }

/*
 * Get argument # 4
 */
  num_siftings = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_num_siftings,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_num_shiftings == NCL_float || type_num_shiftings == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_shiftings must be an integral type (int, uint, long, etc)");
    return(NhlFATAL);
  }
/*
 * Get argument # 5
 */
  rng_seed = (void*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rng_seed,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_rng_seed == NCL_float || type_rng_seed == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: rng_seed must be an integral type (int, uint, long, etc)");
    return(NhlFATAL);
  }

/*
 * Allocate space for tmp_input and tmp_noise_strength, both floating
 * point types.
 */
  if(type_input != NCL_double) {
    type_output = NCL_float;
    tmp_input = (double *)calloc(ntnltnln,sizeof(double));
    if(tmp_input == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_output = NCL_double;
  }
  if(type_noise_strength != NCL_double) {
    tmp_noise_strength = (double *)calloc(1,sizeof(double));
    if(tmp_noise_strength == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing noise_strength array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_output = NCL_double;
  }
/*
 * Allocate space for tmp_ensemble_size.
 */
  tmp_ensemble_size = coerce_input_double(ensemble_size,type_ensemble_size,1,0,NULL,NULL);
  if(tmp_ensemble_size == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_noise_strength.
 */
  tmp_noise_strength = coerce_input_double(noise_strength,type_noise_strength,1,0,NULL,NULL);
  if(tmp_noise_strength == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_S_number.
 */
  tmp_S_number = coerce_input_double(S_number,type_S_number,1,0,NULL,NULL);
  if(tmp_S_number == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_num_siftings.
 */
  tmp_num_siftings = coerce_input_double(num_siftings,type_num_siftings,1,0,NULL,NULL);
  if(tmp_num_siftings == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_rng_seed.
 */
  tmp_rng_seed = coerce_input_double(rng_seed,type_rng_seed,1,0,NULL,NULL);
  if(tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Compute size of leftmost dimensions.
 */
  ndims_output = ndims_input + 1;
  dsizes_output = (ng_size_t*)calloc(ndims_output,sizeof(ng_size_t));  
  if( dsizes_output == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_output[ndims_output-4] = num_imfs;
  dsizes_output[ndims_output-3] = ntim;
  dsizes_output[ndims_output-2] = nlat;
  dsizes_output[ndims_output-2] = nlon;
/* 
 * Allocate space for output array.  For now, because we are not handling 
 * leftmost dimensions, the output size is the input size * num_imfs.
 */  
  size_output = nimntnltnln;
  if(type_output != NCL_double) {
    output = (void *)calloc(size_output, sizeof(float));
    tmp_output = (double *)calloc(nimntnltnln,sizeof(double));
    if(tmp_output == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    output = (void *)calloc(size_output, sizeof(double));
  }
  if(output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Coerce subsection of input (tmp_input) to double if necessary. Normally
 * we would just use coerce_input_double here, but in case we add 
 * leftmost dimensions later, we're using coerce_subset_input_double
 * for now.
 */
  index_input  = 0;
  index_output = 0;
  if(type_input != NCL_double) {
    coerce_subset_input_double(input,tmp_input,index_input,type_input,nlonnlatntim,0,NULL,NULL);
  }
  else {
    tmp_input = &((double*)input)[index_input];
  }
  if(type_output == NCL_double) &((double*)output)[index_output];

/*
 * Call the C routine.
 */
  error_code = ceemdan(tmp_input, &N, tmp_output, &M, tmp_ensemble_size, 
		       tmp_noise_strength, tmp_S_number, tmp_num_siftings, 
		       tmp_rng_seed);
  if(error_code) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: EEMD decomposition routine failed\n");
    return(NhlFATAL);
  }
/*
 * Coerce output back to float if necessary.
 */
  if(type_output == NCL_float) {
    coerce_output_float_only(output,tmp_output,nimntnltnln,index_output);
  }

/*
 * Free unneeded memory.
 */
  if(type_input != NCL_double) NclFree(tmp_input);
  if(type_ensemble_size != NCL_double) NclFree(tmp_ensemble_size);
  if(type_noise_strength != NCL_double) NclFree(tmp_noise_strength);
  if(type_S_number != NCL_double) NclFree(tmp_S_number);
  if(type_num_siftings != NCL_double) NclFree(tmp_num_siftings);
  if(type_rng_seed != NCL_double) NclFree(tmp_rng_seed);
  if(type_output != NCL_double) NclFree(tmp_output);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(output,ndims_output,dsizes_output,NULL,type_output,0);
  NclFree(dsizes_output);
  return(ret);
}

NhlErrorTypes emd_num_imfs_W( void )
{
/*
 * Input variable
 */
  ng_size_t *nt;
  NclBasicDataTypes type_nt;

/*
 * Return variable
 */
  void *Mret;
  ng_size_t dsizes_M[1];
  NclBasicDataTypes type_M;

/*
 * Get argument # 0
 */
  nt = (ng_size_t*)NclGetArgValue(
           0,
           1,
           NULL,
           NULL,
           NULL,
           NULL,
	   &type_nt,
           DONT_CARE);

  if(type_nt != NCL_int && type_nt != NCL_long) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: nt must be an integer or a long");
    return(NhlFATAL);
  }
/* 
 * Allocate space for output array.
 */
#if defined(NG32BIT)
  Mret = (void*)calloc(1, sizeof(int));
#else
  Mret = (void*)calloc(1, sizeof(long));
#endif
  if(Mret == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: Unable to allocate memory for return value");
    return(NhlFATAL);
  }

#if defined(NG32BIT)
  type_M = NCL_int;
  ((int*)Mret)[0] = (int)emd_num_imfs((size_t)(*nt));
#else
  type_M = NCL_long;
  ((long*)Mret)[0] = (long)emd_num_imfs((size_t)(*nt));
#endif
  dsizes_M[0] = 1;
  return(NclReturnValue(Mret,1,dsizes_M,NULL,type_M,0));
}

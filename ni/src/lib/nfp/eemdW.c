/* THIS WRAPPER IS NOT DONE! I'M CHECKING IT IN BEFORE THE SVN TO GIT TRANSITION */

#include <stdio.h>
#include "wrapper.h"
#include "eemd.h"

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
  unsigned int *tmp_ensemble_size = NULL;
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
  void *eemd;
  double *tmp_eemd = NULL;
  int ndims_eemd;
  ng_size_t *dsizes_eemd;
  NclBasicDataTypes type_eemd;

/*
 * Various
 */
  ng_size_t nlon, nlat, ntim; 
  size_t ntnltnln, nimntnltnln;
  ng_size_t size_eemd, index_input, index_eemd;
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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: The input array must be three-dimensional");
    return(NhlFATAL);
  }
  nlat = dsizes_input[ndims_input-3];
  nlon = dsizes_input[ndims_input-2];
  ntim = dsizes_input[ndims_input-1];
  ntnltnln = ntim * nlat * nlon;
  num_imfs = emd_num_imfs((size_t)ntnltnln);
  if(num_imfs == 0 || num_imfs == 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: the input spatial domain must contain at least 3 values.");
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
  if(type_num_siftings == NCL_float || type_num_siftings == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_siftings must be an integral type (int, uint, long, etc)");
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
    type_eemd = NCL_float;
    tmp_input = (double *)calloc(ntnltnln,sizeof(double));
    if(tmp_input == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_eemd = NCL_double;
  }
  if(type_noise_strength != NCL_double) {
    tmp_noise_strength = (double *)calloc(1,sizeof(double));
    if(tmp_noise_strength == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing noise_strength array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_eemd = NCL_double;
  }
/*
 * Allocate space for tmp_ensemble_size.
 */
  tmp_ensemble_size = coerce_input_uint(ensemble_size,type_ensemble_size,1,0,NULL,NULL);
  if(tmp_ensemble_size == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing ensemble_size array to uint");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_noise_strength.
 */
  tmp_noise_strength = coerce_input_double(noise_strength,type_noise_strength,1,0,NULL,NULL);
  if(tmp_noise_strength == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing noise_strength array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_S_number.
 */
  tmp_S_number = coerce_input_uint(S_number,type_S_number,1,0,NULL,NULL);
  if(tmp_S_number == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing S_number array to uint");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_num_siftings.
 */
  tmp_num_siftings = coerce_input_uint(num_siftings,type_num_siftings,1,0,NULL,NULL);
  if(tmp_num_siftings == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input num_siftings to unsigned int");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_rng_seed.
 */
  tmp_rng_seed = coerce_input_ulong(rng_seed,type_rng_seed,1,0,NULL,NULL);
  if(tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input rng_seed to unsigned long");
    return(NhlFATAL);
  }

/*
 * Compute size of leftmost dimensions.
 */
  ndims_eemd = ndims_input + 1;
  dsizes_eemd = (ng_size_t*)calloc(ndims_eemd,sizeof(ng_size_t));  
  if( dsizes_eemd == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_eemd[ndims_eemd-4] = nlat;
  dsizes_eemd[ndims_eemd-3] = nlon;
  dsizes_eemd[ndims_eemd-2] = num_imfs;
  dsizes_eemd[ndims_eemd-1] = ntim;
/* 
 * Allocate space for eemd array.  For now, because we are not handling 
 * leftmost dimensions, the eemd size is the input size * num_imfs.
 */  
  size_eemd = nimntnltnln;
  if(type_eemd != NCL_double) {
    eemd = (void *)calloc(size_eemd, sizeof(float));
    tmp_eemd = (double *)calloc(nimntnltnln,sizeof(double));
    if(tmp_eemd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for temporary eemd array");
      return(NhlFATAL);
    }
  }
  else {
    eemd = (void *)calloc(size_eemd, sizeof(double));
  }
  if(eemd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for eemd array");
    return(NhlFATAL);
  }

/*
 * Coerce subsection of input (tmp_input) to double if necessary. Normally
 * we would just use coerce_input_double here, but in case we add 
 * leftmost dimensions later, we're using coerce_subset_input_double
 * for now.
 */
  index_input  = index_eemd = 0;
  if(type_input != NCL_double) {
    coerce_subset_input_double(input,tmp_input,index_input,type_input,ntnltnln,0,NULL,NULL);
  }
  else {
    tmp_input = &((double*)input)[index_input];
  }
  if(type_eemd == NCL_double) tmp_eemd = &((double*)eemd)[index_eemd];

/*
 * Call the C routine.
 */
  error_code = ceemdan(tmp_input, ntnltnln, tmp_eemd, num_imfs, *tmp_ensemble_size, 
		       *tmp_noise_strength, *tmp_S_number, *tmp_num_siftings, 
		       *tmp_rng_seed);
  if(error_code) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: EEMD decomposition routine failed\n");
    return(NhlFATAL);
  }
/*
 * Coerce eemd back to float if necessary.
 */
  if(type_eemd == NCL_float) {
    coerce_output_float_only(eemd,tmp_eemd,nimntnltnln,index_eemd);
  }

/*
 * Free unneeded memory.
 */
  if(type_input != NCL_double) NclFree(tmp_input);
  if(type_ensemble_size != NCL_uint) NclFree(tmp_ensemble_size);
  if(type_noise_strength != NCL_double) NclFree(tmp_noise_strength);
  if(type_S_number != NCL_uint) NclFree(tmp_S_number);
  if(type_num_siftings != NCL_uint) NclFree(tmp_num_siftings);
  if(type_rng_seed != NCL_ulong) NclFree(tmp_rng_seed);
  if(type_eemd != NCL_double) NclFree(tmp_eemd);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(eemd,ndims_eemd,dsizes_eemd,NULL,type_eemd,0);
  NclFree(dsizes_eemd);
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

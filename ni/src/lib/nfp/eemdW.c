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
  int ndims_input;
  ng_size_t dsizes_input[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_input;

/*
 * Argument # 1
 */
  void *num_imfs;
  ng_size_t tmp_num_imfs;
  NclBasicDataTypes type_num_imfs;

/*
 * Argument # 2
 */
  void *ensemble_size;
  unsigned int *tmp_ensemble_size = NULL;
  NclBasicDataTypes type_ensemble_size;

/*
 * Argument # 3
 */

  void *noise_strength;
  double *tmp_noise_strength = NULL;
  NclBasicDataTypes type_noise_strength;

/*
 * Argument # 4
 */
  void *S_number;
  unsigned int *tmp_S_number = NULL;
  NclBasicDataTypes type_S_number;

/*
 * Argument # 5
 */
  void *num_siftings;
  unsigned int *tmp_num_siftings = NULL;
  NclBasicDataTypes type_num_siftings;

/*
 * Argument # 6
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
  ng_size_t i, nlon, nlat, ntim; 
  size_t nltnln, nimnt, nimntnltnln;
  ng_size_t index_input, index_output;
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
           7,
           &ndims_input,
           dsizes_input,
           NULL,
           NULL,
           &type_input,
           DONT_CARE);

/*
 * Get argument # 1
 */
  num_imfs = (void*)NclGetArgValue(
           1,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_num_imfs,
           DONT_CARE);

  /* The type is prototyped as "numeric", but it must be an int or long */
  if(type_num_imfs != NCL_int && type_num_imfs != NCL_long) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_imfs must be an integer or a long");
    return(NhlFATAL);
  }

/*
 * Get argument # 1
 */
  ensemble_size = (void*)NclGetArgValue(
           2,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_ensemble_size,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_ensemble_size == NCL_float || type_ensemble_size == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: ensemble_size must be an integral value");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  noise_strength = (void*)NclGetArgValue(
           3,
           7,
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
           4,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_S_number,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_S_number == NCL_float || type_S_number == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: S_number must be an integral value");
    return(NhlFATAL);
  }

/*
 * Get argument # 4
 */
  num_siftings = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_num_siftings,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_num_siftings == NCL_float || type_num_siftings == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_siftings must be an integral value");
    return(NhlFATAL);
  }
/*
 * Get argument # 5
 */
  rng_seed = (void*)NclGetArgValue(
           6,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_rng_seed,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_rng_seed == NCL_float || type_rng_seed == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: rng_seed must be an integral value");
    return(NhlFATAL);
  }

  nlat = dsizes_input[ndims_input-3];
  nlon = dsizes_input[ndims_input-2];
  ntim = dsizes_input[ndims_input-1];

/*
 * If num_imfs is 0, then calculate it.
 */
  if(type_num_imfs == NCL_int) {
    tmp_num_imfs = (size_t)(*(int*)num_imfs);
  }
  else {
    tmp_num_imfs = (size_t)(*(long*)num_imfs);
  }
  if(tmp_num_imfs == 0) {
    tmp_num_imfs = emd_num_imfs((size_t)ntim);
  }
  if(tmp_num_imfs < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: the input spatial domain must contain at least 3 values.");
    return(NhlFATAL);
  }
/*
 * These are needed for various array sizes.
 */ 
  nltnln      = nlat * nlon;
  nimnt       = tmp_num_imfs * ntim;
  nimntnltnln = nimnt * nltnln;

/*
 * Allocate space for tmp_input.
 */
  if(type_input != NCL_double) {
    type_eemd = NCL_float;
    tmp_input = (double *)calloc(ntim,sizeof(double));
    if(tmp_input == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_eemd = NCL_double;
  }
/*
 * Coerce various input parameters to their appropriate types
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
 * Error check input parameter values.
 */
  if (*tmp_ensemble_size < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: ensemble_size must be >= 1");
    return(NhlFATAL);
  }
  if (*tmp_S_number < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: S_number must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_siftings must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_S_number == 0 && *tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: one of S_number or num_siftings must be positive");
    return(NhlFATAL);
  }
  if (*tmp_noise_strength < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: noise_strength must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: if S-number !=0 and num_siftings=0, then routine might never finish if stuck in some obscure numerical corner case");
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
  dsizes_eemd[ndims_eemd-2] = tmp_num_imfs;
  dsizes_eemd[ndims_eemd-1] = ntim;
/* 
 * Allocate space for eemd array.
 */  
  if(type_eemd != NCL_double) {
    eemd = (void *)calloc(nimntnltnln, sizeof(float));
    tmp_eemd = (double *)calloc(nimnt,sizeof(double));
    if(tmp_eemd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for temporary eemd array");
      return(NhlFATAL);
    }
  }
  else {
    eemd = (void *)calloc(nimntnltnln, sizeof(double));
  }
  if(eemd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for eemd array");
    return(NhlFATAL);
  }

/*
 * Loop across each lat/lon point and call C function.
 */
  for(i = 0; i < nltnln; i++) {
    index_input  = i*ntim;
    index_output = i*nimnt;
    if(type_input != NCL_double) {
      coerce_subset_input_double(input,tmp_input,index_input,
                                 type_input,ntim,0,NULL,NULL);
    }
    else {
      tmp_input = &((double*)input)[index_input];
    }
    if(type_eemd == NCL_double) {
      tmp_eemd = &((double*)eemd)[index_output];
    }

    error_code = ceemdan(tmp_input, ntim, tmp_eemd, tmp_num_imfs,
                         *tmp_ensemble_size, *tmp_noise_strength, 
                         *tmp_S_number, *tmp_num_siftings, 
                         *tmp_rng_seed);
    if(error_code) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: EEMD decomposition routine failed\n");
      return(NhlFATAL);
    }
    if(type_eemd == NCL_float) {
      coerce_output_float_only(eemd,tmp_eemd,nimnt,index_output);
    }
  }

/*
 * Free unneeded memory.
 */
  if(type_input != NCL_double)          NclFree(tmp_input);
  if(type_ensemble_size != NCL_uint)    NclFree(tmp_ensemble_size);
  if(type_noise_strength != NCL_double) NclFree(tmp_noise_strength);
  if(type_S_number != NCL_uint)         NclFree(tmp_S_number);
  if(type_num_siftings != NCL_uint)     NclFree(tmp_num_siftings);
  if(type_rng_seed != NCL_ulong)        NclFree(tmp_rng_seed);
  if(type_eemd != NCL_double)           NclFree(tmp_eemd);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(eemd,ndims_eemd,dsizes_eemd,NULL,type_eemd,0);
  NclFree(dsizes_eemd);
  return(ret);
}

NhlErrorTypes ceemdan_opt_W( void )
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
  logical *opt;
/*
 * Attributes
 */
  size_t tmp_num_imfs;
  unsigned int *tmp_ensemble_size=NULL, *tmp_S_number=NULL, *tmp_num_siftings=NULL;
  unsigned int def_ensemble_size=250, def_S_number=0, def_num_siftings=0;;
  unsigned long int *tmp_rng_seed=NULL, def_rng_seed=0;
  double *tmp_noise_strength=NULL, def_noise_strength=0.2;
  int set_num_imfs=0, set_ensemble_size=0, set_S_number=0;
  int  set_num_siftings=0, set_rng_seed=0, set_noise_strength=0;
  NclBasicDataTypes type_ensemble_size, type_noise_strength, type_S_number;
  NclBasicDataTypes type_num_siftings, type_rng_seed;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Return variable
 */
  void *eemd;
  double *tmp_eemd=NULL;
  int ndims_eemd;
  ng_size_t *dsizes_eemd;
  NclBasicDataTypes type_eemd;

/*
 * Various
 */
  ng_size_t i, nlon, nlat, ntim; 
  size_t nltnln, nimnt, nimntnltnln;
  ng_size_t index_input, index_output;
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
           2,
           &ndims_input,
           dsizes_input,
           NULL,
           NULL,
           &type_input,
           DONT_CARE);

  nlat = dsizes_input[ndims_input-3];
  nlon = dsizes_input[ndims_input-2];
  ntim = dsizes_input[ndims_input-1];

/*
 * Get argument # 1
 */
  opt = (logical *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
  if(*opt) {
    stack_entry = _NclGetArg(1, 2, DONT_CARE);
    switch (stack_entry.kind) {
    case NclStk_VAR:
      if (stack_entry.u.data_var->var.att_id != -1) {
        attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
        if (attr_obj == NULL) {
          break;
        }
      }
      else {
/*
 * att_id == -1 ==> no optional args given.
 */
        break;
      }
/* 
 * Get optional arguments.
 */
      if (attr_obj->att.n_atts > 0) {
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them. The current ones 
 * recognized are:
 *
 *   "num_imfs"
 *   "ensemble_size"
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 *   "noise_strength"
 */
        while (attr_list != NULL) {
/* num_imfs */
          if (!strcmp(attr_list->attname, "num_imfs")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int && 
               attr_list->attvalue->multidval.data_type != NCL_long) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: The 'num_imfs' attribute must be an integer or long value");
              return(NhlFATAL);
            }
            else {
              if(attr_list->attvalue->multidval.data_type == NCL_int) {
                tmp_num_imfs = (size_t)(*(int*)attr_list->attvalue->multidval.val);
              }
              else {
                tmp_num_imfs = (size_t)(*(long*)attr_list->attvalue->multidval.val);
              }
              set_num_imfs = 1;
            }
          }
/* ensemble_size */
          if (!strcmp(attr_list->attname, "ensemble_size")) {
            type_ensemble_size = attr_list->attvalue->multidval.data_type;
            if(type_ensemble_size != NCL_byte  && type_ensemble_size == NCL_ubyte &&
               type_ensemble_size != NCL_short && type_ensemble_size == NCL_ushort &&
               type_ensemble_size != NCL_int   && type_ensemble_size == NCL_uint &&
               type_ensemble_size != NCL_long  && type_ensemble_size == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: The 'ensemble_size' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_ensemble_size = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                    type_ensemble_size,1,0,NULL,NULL);
              set_ensemble_size = 1;
            }
          }
/* S_number */
          if (!strcmp(attr_list->attname, "S_number")) {
            type_S_number = attr_list->attvalue->multidval.data_type;
            if(type_S_number != NCL_byte  && type_S_number == NCL_ubyte &&
               type_S_number != NCL_short && type_S_number == NCL_ushort &&
               type_S_number != NCL_int   && type_S_number == NCL_uint &&
               type_S_number != NCL_long  && type_S_number == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: The 'S_number' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_S_number = coerce_input_uint(attr_list->attvalue->multidval.val,
                                               type_S_number,1,0,NULL,NULL);
              set_S_number = 1;
            }
          }
/* num_siftings */
          if (!strcmp(attr_list->attname, "num_siftings")) {
            type_num_siftings = attr_list->attvalue->multidval.data_type;
            if(type_num_siftings != NCL_byte  && type_num_siftings == NCL_ubyte &&
               type_num_siftings != NCL_short && type_num_siftings == NCL_ushort &&
               type_num_siftings != NCL_int   && type_num_siftings == NCL_uint &&
               type_num_siftings != NCL_long  && type_num_siftings == NCL_ulong) {

            }
            else {
              tmp_num_siftings = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                   type_num_siftings,1,0,NULL,NULL);
              set_num_siftings = 1;
            }
          }
/* rng_seed */
          if (!strcmp(attr_list->attname, "rng_seed")) {
            type_rng_seed = attr_list->attvalue->multidval.data_type;
            if(type_rng_seed != NCL_byte  && type_rng_seed == NCL_ubyte &&
               type_rng_seed != NCL_short && type_rng_seed == NCL_ushort &&
               type_rng_seed != NCL_int   && type_rng_seed == NCL_uint &&
               type_rng_seed != NCL_long  && type_rng_seed == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: The 'rng_seed' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_rng_seed = coerce_input_ulong(attr_list->attvalue->multidval.val,
                                                type_rng_seed,1,0,NULL,NULL);
              set_rng_seed = 1;
            }
          }
/*
 * "noise_strength
 */
          if (!strcmp(attr_list->attname, "noise_strength")) {
            type_noise_strength = attr_list->attvalue->multidval.data_type;
            tmp_noise_strength = coerce_input_double(attr_list->attvalue->multidval.val,
                                                       type_noise_strength,1,0,NULL,NULL);
            set_noise_strength = 1;
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

/*
 * If any attributes not set, then set to default values.
 */
  if(!set_num_imfs) {
    tmp_num_imfs = emd_num_imfs((size_t)ntim);
  }
  if(tmp_num_imfs == 0 || tmp_num_imfs == 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: num_imfs must contain at least 3 values.");
    return(NhlFATAL);
  }
  if(!set_ensemble_size || tmp_ensemble_size == NULL) {
    tmp_ensemble_size = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_ensemble_size = def_ensemble_size;
  }
  if(!set_S_number || tmp_S_number == NULL) {
    tmp_S_number = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_S_number = def_S_number;
  }
  if(!set_num_siftings || tmp_num_siftings == NULL) {
    tmp_num_siftings = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_num_siftings = def_num_siftings;
  }
  if(!set_rng_seed || tmp_rng_seed == NULL) {
    tmp_rng_seed = (unsigned long *)malloc(sizeof(unsigned long));
    *tmp_rng_seed = def_rng_seed;
  }
  if(!set_noise_strength || tmp_noise_strength == NULL) {
    tmp_noise_strength = (double *)malloc(sizeof(double));
    *tmp_noise_strength = def_noise_strength;
  }
  if(tmp_ensemble_size == NULL || tmp_noise_strength == NULL || tmp_S_number == NULL || 
     tmp_num_siftings == NULL || tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: Unable to allocate memory for various input parameters");
    return(NhlFATAL);
  }

/*
 * Error check input parameter values.
 */
  if (*tmp_ensemble_size < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: ensemble_size must be >= 1");
    return(NhlFATAL);
  }
  if (*tmp_S_number < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: S_number must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: num_siftings must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_S_number == 0 && *tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: one of S_number or num_siftings must be positive");
    return(NhlFATAL);
  }
  if (*tmp_noise_strength < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: noise_strength must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: if S-number !=0 and num_siftings=0, then routine might never finish if stuck in some obscure numerical corner case");
    return(NhlFATAL);
  }    

/*
 * These are needed for various array sizes.
 */ 
  nltnln      = nlat * nlon;
  nimnt       = tmp_num_imfs * ntim;
  nimntnltnln = nimnt * nltnln;
/*
 * Allocate space for tmp_input and tmp_noise_strength, both floating
 * point types.
 */
  if(type_input != NCL_double) {
    type_eemd = NCL_float;
    tmp_input = (double *)calloc(ntim,sizeof(double));
    if(tmp_input == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_eemd = NCL_double;
  }
/*
 * Compute size of leftmost dimensions.
 */
  ndims_eemd = ndims_input + 1;
  dsizes_eemd = (ng_size_t*)calloc(ndims_eemd,sizeof(ng_size_t));  
  if( dsizes_eemd == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_eemd[ndims_eemd-4] = nlat;
  dsizes_eemd[ndims_eemd-3] = nlon;
  dsizes_eemd[ndims_eemd-2] = tmp_num_imfs;
  dsizes_eemd[ndims_eemd-1] = ntim;
/* 
 * Allocate space for eemd array.
 */  
  if(type_eemd != NCL_double) {
    eemd = (void *)calloc(nimntnltnln, sizeof(float));
    tmp_eemd = (double *)calloc(nimnt,sizeof(double));
    if(tmp_eemd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: Unable to allocate memory for temporary eemd array");
      return(NhlFATAL);
    }
  }
  else {
    eemd = (void *)calloc(nimntnltnln, sizeof(double));
  }
  if(eemd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: Unable to allocate memory for eemd array");
    return(NhlFATAL);
  }

/*
 * Loop across each lat/lon point and call C function.
 */
  for(i = 0; i < nltnln; i++) {
    index_input  = i*ntim;
    index_output = i*nimnt;
    if(type_input != NCL_double) {
      coerce_subset_input_double(input,tmp_input,index_input,
                                 type_input,ntim,0,NULL,NULL);
    }
    else {
      tmp_input = &((double*)input)[index_input];
    }
    if(type_eemd == NCL_double) {
      tmp_eemd = &((double*)eemd)[index_output];
    }

    error_code = ceemdan(tmp_input, ntim, tmp_eemd, tmp_num_imfs,
                         *tmp_ensemble_size, *tmp_noise_strength, 
                         *tmp_S_number, *tmp_num_siftings, 
                         *tmp_rng_seed);
    if(error_code) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan_opt: EEMD decomposition routine failed\n");
      return(NhlFATAL);
    }
/* Coerce eemd back to float if necessary. */
    if(type_eemd == NCL_float) {
      coerce_output_float_only(eemd,tmp_eemd,nimnt,index_output);
    }
  }
/*
 * Free unneeded memory.
 */
  if(type_input != NCL_double)                                 NclFree(tmp_input);
  if(!set_ensemble_size  || type_ensemble_size != NCL_uint)    NclFree(tmp_ensemble_size);
  if(!set_noise_strength || type_noise_strength != NCL_double) NclFree(tmp_noise_strength);
  if(!set_S_number       || type_S_number != NCL_uint)         NclFree(tmp_S_number);
  if(!set_num_siftings   || type_num_siftings != NCL_uint)     NclFree(tmp_num_siftings);
  if(!set_rng_seed       || type_rng_seed != NCL_ulong)        NclFree(tmp_rng_seed);
  if(type_eemd != NCL_double)                                  NclFree(tmp_eemd);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(eemd,ndims_eemd,dsizes_eemd,NULL,type_eemd,0);
  NclFree(dsizes_eemd);
  return(ret);
}

NhlErrorTypes eemd_opt_W( void )
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
  logical *opt;
/*
 * Attributes
 */
  size_t tmp_num_imfs;
  unsigned int *tmp_ensemble_size=NULL, *tmp_S_number=NULL, *tmp_num_siftings=NULL;
  unsigned int def_ensemble_size=250, def_S_number=0, def_num_siftings=0;;
  unsigned long int *tmp_rng_seed=NULL, def_rng_seed=0;
  double *tmp_noise_strength=NULL, def_noise_strength=0.2;
  int set_num_imfs=0, set_ensemble_size=0, set_S_number=0;
  int  set_num_siftings=0, set_rng_seed=0, set_noise_strength=0;
  NclBasicDataTypes type_ensemble_size, type_noise_strength, type_S_number;
  NclBasicDataTypes type_num_siftings, type_rng_seed;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Return variable
 */
  void *eemd_output;
  double *tmp_eemd_output=NULL;
  int ndims_eemd_output;
  ng_size_t *dsizes_eemd_output;
  NclBasicDataTypes type_eemd_output;

/*
 * Various
 */
  ng_size_t i, nlon, nlat, ntim; 
  size_t nltnln, nimnt, nimntnltnln;
  ng_size_t index_input, index_output;
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
           2,
           &ndims_input,
           dsizes_input,
           NULL,
           NULL,
           &type_input,
           DONT_CARE);

  nlat = dsizes_input[ndims_input-3];
  nlon = dsizes_input[ndims_input-2];
  ntim = dsizes_input[ndims_input-1];

/*
 * Get argument # 1
 */
  opt = (logical *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
  if(*opt) {
    stack_entry = _NclGetArg(1, 2, DONT_CARE);
    switch (stack_entry.kind) {
    case NclStk_VAR:
      if (stack_entry.u.data_var->var.att_id != -1) {
        attr_obj = (NclAtt) _NclGetObj(stack_entry.u.data_var->var.att_id);
        if (attr_obj == NULL) {
          break;
        }
      }
      else {
/*
 * att_id == -1 ==> no optional args given.
 */
        break;
      }
/* 
 * Get optional arguments.
 */
      if (attr_obj->att.n_atts > 0) {
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them. The current ones 
 * recognized are:
 *
 *   "num_imfs"
 *   "ensemble_size"
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 *   "noise_strength"
 */
        while (attr_list != NULL) {
/* num_imfs */
          if (!strcmp(attr_list->attname, "num_imfs")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int && 
               attr_list->attvalue->multidval.data_type != NCL_long) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: The 'num_imfs' attribute must be an integer or long value");
              return(NhlFATAL);
            }
            else {
              if(attr_list->attvalue->multidval.data_type == NCL_int) {
                tmp_num_imfs = (size_t)(*(int*)attr_list->attvalue->multidval.val);
              }
              else {
                tmp_num_imfs = (size_t)(*(long*)attr_list->attvalue->multidval.val);
              }
              set_num_imfs = 1;
            }
          }
/* ensemble_size */
          if (!strcmp(attr_list->attname, "ensemble_size")) {
            type_ensemble_size = attr_list->attvalue->multidval.data_type;
            if(type_ensemble_size != NCL_byte  && type_ensemble_size == NCL_ubyte &&
               type_ensemble_size != NCL_short && type_ensemble_size == NCL_ushort &&
               type_ensemble_size != NCL_int   && type_ensemble_size == NCL_uint &&
               type_ensemble_size != NCL_long  && type_ensemble_size == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: The 'ensemble_size' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_ensemble_size = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                    type_ensemble_size,1,0,NULL,NULL);
              set_ensemble_size = 1;
            }
          }
/* S_number */
          if (!strcmp(attr_list->attname, "S_number")) {
            type_S_number = attr_list->attvalue->multidval.data_type;
            if(type_S_number != NCL_byte  && type_S_number == NCL_ubyte &&
               type_S_number != NCL_short && type_S_number == NCL_ushort &&
               type_S_number != NCL_int   && type_S_number == NCL_uint &&
               type_S_number != NCL_long  && type_S_number == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: The 'S_number' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_S_number = coerce_input_uint(attr_list->attvalue->multidval.val,
                                               type_S_number,1,0,NULL,NULL);
              set_S_number = 1;
            }
          }
/* num_siftings */
          if (!strcmp(attr_list->attname, "num_siftings")) {
            type_num_siftings = attr_list->attvalue->multidval.data_type;
            if(type_num_siftings != NCL_byte  && type_num_siftings == NCL_ubyte &&
               type_num_siftings != NCL_short && type_num_siftings == NCL_ushort &&
               type_num_siftings != NCL_int   && type_num_siftings == NCL_uint &&
               type_num_siftings != NCL_long  && type_num_siftings == NCL_ulong) {

            }
            else {
              tmp_num_siftings = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                   type_num_siftings,1,0,NULL,NULL);
              set_num_siftings = 1;
            }
          }
/* rng_seed */
          if (!strcmp(attr_list->attname, "rng_seed")) {
            type_rng_seed = attr_list->attvalue->multidval.data_type;
            if(type_rng_seed != NCL_byte  && type_rng_seed == NCL_ubyte &&
               type_rng_seed != NCL_short && type_rng_seed == NCL_ushort &&
               type_rng_seed != NCL_int   && type_rng_seed == NCL_uint &&
               type_rng_seed != NCL_long  && type_rng_seed == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: The 'rng_seed' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_rng_seed = coerce_input_ulong(attr_list->attvalue->multidval.val,
                                                type_rng_seed,1,0,NULL,NULL);
              set_rng_seed = 1;
            }
          }
/*
 * "noise_strength
 */
          if (!strcmp(attr_list->attname, "noise_strength")) {
            type_noise_strength = attr_list->attvalue->multidval.data_type;
            tmp_noise_strength = coerce_input_double(attr_list->attvalue->multidval.val,
                                                       type_noise_strength,1,0,NULL,NULL);
            set_noise_strength = 1;
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

/*
 * If any attributes not set, then set to default values.
 */
  if(!set_num_imfs) {
    tmp_num_imfs = emd_num_imfs((size_t)ntim);
  }
  if(tmp_num_imfs == 0 || tmp_num_imfs == 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: num_imfs must contain at least 3 values.");
    return(NhlFATAL);
  }
  if(!set_ensemble_size || tmp_ensemble_size == NULL) {
    tmp_ensemble_size = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_ensemble_size = def_ensemble_size;
  }
  if(!set_S_number || tmp_S_number == NULL) {
    tmp_S_number = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_S_number = def_S_number;
  }
  if(!set_num_siftings || tmp_num_siftings == NULL) {
    tmp_num_siftings = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_num_siftings = def_num_siftings;
  }
  if(!set_rng_seed || tmp_rng_seed == NULL) {
    tmp_rng_seed = (unsigned long *)malloc(sizeof(unsigned long));
    *tmp_rng_seed = def_rng_seed;
  }
  if(!set_noise_strength || tmp_noise_strength == NULL) {
    tmp_noise_strength = (double *)malloc(sizeof(double));
    *tmp_noise_strength = def_noise_strength;
  }
  if(tmp_ensemble_size == NULL || tmp_noise_strength == NULL || tmp_S_number == NULL || 
     tmp_num_siftings == NULL || tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: Unable to allocate memory for various input parameters");
    return(NhlFATAL);
  }

/*
 * Error check input parameter values.
 */
  if (*tmp_ensemble_size < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: ensemble_size must be >= 1");
    return(NhlFATAL);
  }
  if (*tmp_S_number < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: S_number must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: num_siftings must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_S_number == 0 && *tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: one of S_number or num_siftings must be positive");
    return(NhlFATAL);
  }
  if (*tmp_noise_strength < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: noise_strength must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: if S-number !=0 and num_siftings=0, then routine might never finish if stuck in some obscure numerical corner case");
    return(NhlFATAL);
  }    

/*
 * These are needed for various array sizes.
 */ 
  nltnln      = nlat * nlon;
  nimnt       = tmp_num_imfs * ntim;
  nimntnltnln = nimnt * nltnln;
/*
 * Allocate space for tmp_input and tmp_noise_strength, both floating
 * point types.
 */
  if(type_input != NCL_double) {
    type_eemd_output = NCL_float;
    tmp_input = (double *)calloc(ntim,sizeof(double));
    if(tmp_input == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_eemd_output = NCL_double;
  }
/*
 * Compute size of leftmost dimensions.
 */
  ndims_eemd_output = ndims_input + 1;
  dsizes_eemd_output = (ng_size_t*)calloc(ndims_eemd_output,sizeof(ng_size_t));  
  if( dsizes_eemd_output == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_eemd_output[ndims_eemd_output-4] = nlat;
  dsizes_eemd_output[ndims_eemd_output-3] = nlon;
  dsizes_eemd_output[ndims_eemd_output-2] = tmp_num_imfs;
  dsizes_eemd_output[ndims_eemd_output-1] = ntim;
/* 
 * Allocate space for eemd_output array.
 */  
  if(type_eemd_output != NCL_double) {
    eemd_output = (void *)calloc(nimntnltnln, sizeof(float));
    tmp_eemd_output = (double *)calloc(nimnt,sizeof(double));
    if(tmp_eemd_output == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: Unable to allocate memory for temporary eemd array");
      return(NhlFATAL);
    }
  }
  else {
    eemd_output = (void *)calloc(nimntnltnln, sizeof(double));
  }
  if(eemd_output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: Unable to allocate memory for eemd array");
    return(NhlFATAL);
  }

/*
 * Loop across each lat/lon point and call C function.
 */
  for(i = 0; i < nltnln; i++) {
    index_input  = i*ntim;
    index_output = i*nimnt;
    if(type_input != NCL_double) {
      coerce_subset_input_double(input,tmp_input,index_input,
                                 type_input,ntim,0,NULL,NULL);
    }
    else {
      tmp_input = &((double*)input)[index_input];
    }
    if(type_eemd_output == NCL_double) {
      tmp_eemd_output = &((double*)eemd_output)[index_output];
    }

    error_code = eemd(tmp_input, ntim, tmp_eemd_output, tmp_num_imfs,
		      *tmp_ensemble_size, *tmp_noise_strength, 
		      *tmp_S_number, *tmp_num_siftings, 
		      *tmp_rng_seed);
    if(error_code) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd_opt: EEMD decomposition routine failed\n");
      return(NhlFATAL);
    }
/* Coerce eemd_output back to float if necessary. */
    if(type_eemd_output == NCL_float) {
      coerce_output_float_only(eemd_output,tmp_eemd_output,nimnt,index_output);
    }
  }
/*
 * Free unneeded memory.
 */
  if(type_input != NCL_double)                                 NclFree(tmp_input);
  if(!set_ensemble_size  || type_ensemble_size != NCL_uint)    NclFree(tmp_ensemble_size);
  if(!set_noise_strength || type_noise_strength != NCL_double) NclFree(tmp_noise_strength);
  if(!set_S_number       || type_S_number != NCL_uint)         NclFree(tmp_S_number);
  if(!set_num_siftings   || type_num_siftings != NCL_uint)     NclFree(tmp_num_siftings);
  if(!set_rng_seed       || type_rng_seed != NCL_ulong)        NclFree(tmp_rng_seed);
  if(type_eemd_output != NCL_double)                           NclFree(tmp_eemd_output);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(eemd_output,ndims_eemd_output,dsizes_eemd_output,NULL,type_eemd_output,0);
  NclFree(dsizes_eemd_output);
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

  if(type_nt == NCL_float || type_nt == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: nt must be an integeral value");
    return(NhlFATAL);
  }
/* 
 * Allocate space for output array.
 */
#if defined(NG32BIT)
  Mret = (void*)calloc(1, sizeof(int));
  if(Mret == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: Unable to allocate memory for return value");
    return(NhlFATAL);
  }
  type_M = NCL_int;
  ((int*)Mret)[0] = (int)emd_num_imfs((size_t)(*nt));
#else
  Mret = (void*)calloc(1, sizeof(long));
  if(Mret == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"emd_num_imfs: Unable to allocate memory for return value");
    return(NhlFATAL);
  }
  type_M = NCL_long;
  ((long*)Mret)[0] = (long)emd_num_imfs((size_t)(*nt));
#endif
  dsizes_M[0] = 1;
  return(NclReturnValue(Mret,1,dsizes_M,NULL,type_M,0));
}

#include <stdio.h>
#include "wrapper.h"
#include "eemd.h"


/*
 * IMPORTANT NOTE:  The ceemdan and eemd wrappers are identical, except for 
 * the routine names.  If you update one, you likely should update the other
 * one with the same changes.
 */
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
  void *ensemble_size;
  unsigned int *tmp_ensemble_size = NULL;
  NclBasicDataTypes type_ensemble_size;

/*
 * Argument # 2
 */
  void *num_imfs;
  ng_size_t tmp_num_imfs;
  NclBasicDataTypes type_num_imfs;

/*
 * Argument # 3
 */

  void *noise_strength;
  double *tmp_noise_strength = NULL;
  NclBasicDataTypes type_noise_strength;

/*
 * Argument # 4
 */
  logical *opt;

/*
 * Argument # 5
 */
  int *dims;
  ng_size_t ndims;

/*
 * Attributes
 */
  unsigned int *tmp_S_number=NULL, *tmp_num_siftings=NULL;
  unsigned int def_S_number=4, def_num_siftings=50;
  unsigned long int *tmp_rng_seed=NULL, def_rng_seed=0;
  logical set_S_number=False, set_num_siftings=False, set_rng_seed=False;
  NclBasicDataTypes type_S_number=NCL_uint, type_num_siftings=NCL_uint, type_rng_seed=NCL_ulong;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Return variable
 */
  void *output;
  double *tmp_output=NULL;
  int ndims_output;
  ng_size_t *dsizes_output;
  NclBasicDataTypes type_output;

/*
 * Various
 */
  ng_size_t i, j, k, ntim, total_nl, total_nr;
  size_t npts, nrnt, ntnpts, nimnt, nimntnpts;
  ng_size_t index_nrnt, index_input, index_output;
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
 * Get argument # 1
 */
  num_imfs = (void*)NclGetArgValue(
           1,
           6,
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
 * Get argument # 2
 */
  ensemble_size = (void*)NclGetArgValue(
           2,
           6,
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
 * Get argument # 3
 */
  noise_strength = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_noise_strength,
           DONT_CARE);

/*
 * Get argument # 4
 */
  opt = (logical *)NclGetArgValue(
            4,
            6, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

/*
 * Get argument # 5
 */
  dims = (int *)NclGetArgValue(5,6,NULL,&ndims,NULL,NULL,NULL,DONT_CARE);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
  if(ndims > ndims_input) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: too many dimensions in dimension argument, can't continue");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_input) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Invalid dimension argument, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Calculate number of leftmost, rightmost, and middle elements.
 * The "middle" elements are the time dimensions.  Usually there
 * is just one time dimension, but we are allowing for multiple
 * ones here.
 */
  ntim = total_nl = total_nr = 1;
  for(i = 0; i < ndims ; i++) ntim = ntim*dsizes_input[dims[i]];
  for(i = 0; i < dims[0]; i++) total_nl *= dsizes_input[i];
  for(i = dims[ndims-1]+1; i < ndims_input; i++) total_nr *= dsizes_input[i];
  npts = total_nl * total_nr;   /* This is usually the lat x lon dimensions */

/*
 * Deal with num_imfs. If it is 0, then calculate it.
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
  if(tmp_num_imfs < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: num_imfs must be >= 1");
    return(NhlFATAL);
  }

/*
 * Coerce various input parameters to their appropriate types
 */
  tmp_ensemble_size = coerce_input_uint(ensemble_size,type_ensemble_size,1,0,NULL,NULL);
  if(tmp_ensemble_size == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing ensemble_size array to uint");
    return(NhlFATAL);
  }
  if (*tmp_ensemble_size < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: ensemble_size must be >= 1");
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
  if (*tmp_noise_strength < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: noise_strength must be non-negative");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
 * The current ones that are recognized are:
 *
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 */
  if(*opt) {
    stack_entry = _NclGetArg(4, 6, DONT_CARE);
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
 * Loop through attributes and check them. The current ones that
 * are recognized are:
 *
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 */
        while (attr_list != NULL) {
/* S_number */
          if (!strcmp(attr_list->attname, "S_number")) {
            type_S_number = attr_list->attvalue->multidval.data_type;
            if(type_S_number != NCL_byte  && type_S_number == NCL_ubyte &&
               type_S_number != NCL_short && type_S_number == NCL_ushort &&
               type_S_number != NCL_int   && type_S_number == NCL_uint &&
               type_S_number != NCL_long  && type_S_number == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: The 'S_number' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_S_number = coerce_input_uint(attr_list->attvalue->multidval.val,
                                               type_S_number,1,0,NULL,NULL);
              set_S_number = True;
            }
          }
/* num_siftings */
          if (!strcmp(attr_list->attname, "num_siftings")) {
            type_num_siftings = attr_list->attvalue->multidval.data_type;
            if(type_num_siftings != NCL_byte  && type_num_siftings == NCL_ubyte &&
               type_num_siftings != NCL_short && type_num_siftings == NCL_ushort &&
               type_num_siftings != NCL_int   && type_num_siftings == NCL_uint &&
               type_num_siftings != NCL_long  && type_num_siftings == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: The 'num_siftings' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_num_siftings = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                   type_num_siftings,1,0,NULL,NULL);
              set_num_siftings = True;
            }
          }
/* rng_seed */
          if (!strcmp(attr_list->attname, "rng_seed")) {
            type_rng_seed = attr_list->attvalue->multidval.data_type;
            if(type_rng_seed != NCL_byte  && type_rng_seed == NCL_ubyte &&
               type_rng_seed != NCL_short && type_rng_seed == NCL_ushort &&
               type_rng_seed != NCL_int   && type_rng_seed == NCL_uint &&
               type_rng_seed != NCL_long  && type_rng_seed == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: The 'rng_seed' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_rng_seed = coerce_input_ulong(attr_list->attvalue->multidval.val,
                                                type_rng_seed,1,0,NULL,NULL);
              set_rng_seed = True;
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

/*
 * Set default values for various parameters, if not set by user.
 */
  if(!set_S_number) {
    tmp_S_number = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_S_number = def_S_number;
  }
  if(!set_num_siftings) {
    tmp_num_siftings = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_num_siftings = def_num_siftings;
  }
  if(!set_rng_seed) {
    tmp_rng_seed = (unsigned long *)malloc(sizeof(unsigned long));
    *tmp_rng_seed = def_rng_seed;
  }
  if(tmp_S_number == NULL || tmp_num_siftings == NULL || tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for various input parameters");
    return(NhlFATAL);
  }

/*
 * Error check attribute parameter values.
 */
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
  if (*tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: if S-number !=0 and num_siftings=0, then routine might never finish if stuck in some obscure numerical corner case");
    return(NhlFATAL);
  }    

/*
 * Some of these are needed for various array sizes.
 */
  ntnpts    = ntim * npts;
  nimnt     = tmp_num_imfs * ntim;
  nimntnpts = tmp_num_imfs * ntnpts;

/*
 * Allocate space for input array no matter what, because it 
 * may not be contiguous in memory.
 */
  tmp_input = (double *)calloc(ntim,sizeof(double));
  if(tmp_input == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
  if(type_input != NCL_double) type_output = NCL_float;
  else                         type_output = NCL_double;

/*
 * Compute size of output dimensions.
 */
  ndims_output = ndims_input + 1;
  dsizes_output = (ng_size_t*)calloc(ndims_output,sizeof(ng_size_t));  
  if( dsizes_output == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_output[0] = tmp_num_imfs;
  for(i=1; i<ndims_output; i++) dsizes_output[i] = dsizes_input[i-1];

/* 
 * Allocate space for output array.
 */  
  tmp_output = (double *)calloc(nimnt,sizeof(double));
  if(tmp_output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for temporary output array");
    return(NhlFATAL);
  }
  if(type_output != NCL_double) output = (void *)calloc(nimntnpts, sizeof(float));
  else                          output = (void *)calloc(nimntnpts, sizeof(double));
  if(output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost/rightmost dimensions and call the
 * C routine for each subsection of the  input arrays.
 */
  nrnt = total_nr * ntim;
  for(i = 0; i < total_nl; i++) {
    index_nrnt = i*nrnt;
    for(j = 0; j < total_nr; j++) {
      index_input = index_nrnt + j;

      /* Coerce ntim subsection of input to double */
      coerce_subset_input_double_step(input,tmp_input,index_input,total_nr,
                                      type_input,ntim,0,NULL,NULL);
      
      error_code = ceemdan(tmp_input, ntim, tmp_output, tmp_num_imfs,
                           *tmp_ensemble_size, *tmp_noise_strength, 
                           *tmp_S_number, *tmp_num_siftings, 
                           *tmp_rng_seed);
      if(error_code) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"ceemdan: EEMD decomposition routine failed\n");
        return(NhlFATAL);
      }
      /* Coerce (num_imfs x ntim) subsection of output back to appropriate type.
         The C routine returns eemd as num_imfs x ntim (in memory), so we need
         to copy to the correction index locations in the output array. */
      for(k = 0; k < tmp_num_imfs; k++) {
        index_output  = (k*ntnpts) + index_input;
        coerce_output_float_or_double_step(output,&tmp_output[k*ntim],type_output,ntim,
                                           index_output,total_nr);
      }
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(tmp_input);
  NclFree(tmp_output);
  if(type_ensemble_size  != NCL_uint)                          NclFree(tmp_ensemble_size);
  if(type_noise_strength != NCL_double)                        NclFree(tmp_noise_strength);
  if(!set_S_number       || type_S_number       != NCL_uint)   NclFree(tmp_S_number);
  if(!set_num_siftings   || type_num_siftings   != NCL_uint)   NclFree(tmp_num_siftings);
  if(!set_rng_seed       || type_rng_seed       != NCL_ulong)  NclFree(tmp_rng_seed);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(output,ndims_output,dsizes_output,NULL,type_output,0);
  NclFree(dsizes_output);
  return(ret);
}


NhlErrorTypes eemd_W( void )
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
  void *ensemble_size;
  unsigned int *tmp_ensemble_size = NULL;
  NclBasicDataTypes type_ensemble_size;

/*
 * Argument # 2
 */
  void *num_imfs;
  ng_size_t tmp_num_imfs;
  NclBasicDataTypes type_num_imfs;

/*
 * Argument # 3
 */

  void *noise_strength;
  double *tmp_noise_strength = NULL;
  NclBasicDataTypes type_noise_strength;

/*
 * Argument # 4
 */
  logical *opt;

/*
 * Argument # 5
 */
  int *dims;
  ng_size_t ndims;

/*
 * Attributes
 */
  unsigned int *tmp_S_number=NULL, *tmp_num_siftings=NULL;
  unsigned int def_S_number=4, def_num_siftings=50;
  unsigned long int *tmp_rng_seed=NULL, def_rng_seed=0;
  logical set_S_number=False, set_num_siftings=False, set_rng_seed=False;
  NclBasicDataTypes type_S_number=NCL_uint, type_num_siftings=NCL_uint, type_rng_seed=NCL_ulong;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Return variable
 */
  void *output;
  double *tmp_output=NULL;
  int ndims_output;
  ng_size_t *dsizes_output;
  NclBasicDataTypes type_output;

/*
 * Various
 */
  ng_size_t i, j, k, ntim, total_nl, total_nr;
  size_t npts, nrnt, ntnpts, nimnt, nimntnpts;
  ng_size_t index_nrnt, index_input, index_output;
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
 * Get argument # 1
 */
  num_imfs = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_num_imfs,
           DONT_CARE);

  /* The type is prototyped as "numeric", but it must be an int or long */
  if(type_num_imfs != NCL_int && type_num_imfs != NCL_long) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: num_imfs must be an integer or a long");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  ensemble_size = (void*)NclGetArgValue(
           2,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_ensemble_size,
           DONT_CARE);

  /* The type is prototyped as "numeric", but float and double are not allowed here */
  if(type_ensemble_size == NCL_float || type_ensemble_size == NCL_double) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: ensemble_size must be an integral value");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  noise_strength = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_noise_strength,
           DONT_CARE);

/*
 * Get argument # 4
 */
  opt = (logical *)NclGetArgValue(
            4,
            6, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

/*
 * Get argument # 5
 */
  dims = (int *)NclGetArgValue(5,6,NULL,&ndims,NULL,NULL,NULL,DONT_CARE);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
  if(ndims > ndims_input) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: too many dimensions in dimension argument, can't continue");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_input) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Invalid dimension argument, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Calculate number of leftmost, rightmost, and middle elements.
 * The "middle" elements are the time dimensions.  Usually there
 * is just one time dimension, but we are allowing for multiple
 * ones here.
 */
  ntim = total_nl = total_nr = 1;
  for(i = 0; i < ndims ; i++) ntim = ntim*dsizes_input[dims[i]];
  for(i = 0; i < dims[0]; i++) total_nl *= dsizes_input[i];
  for(i = dims[ndims-1]+1; i < ndims_input; i++) total_nr *= dsizes_input[i];
  npts = total_nl * total_nr;   /* This is usually the lat x lon dimensions */

/*
 * Deal with num_imfs. If it is 0, then calculate it.
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
  if(tmp_num_imfs < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: num_imfs must be >= 1");
    return(NhlFATAL);
  }

/*
 * Coerce various input parameters to their appropriate types
 */
  tmp_ensemble_size = coerce_input_uint(ensemble_size,type_ensemble_size,1,0,NULL,NULL);
  if(tmp_ensemble_size == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for coercing ensemble_size array to uint");
    return(NhlFATAL);
  }
  if (*tmp_ensemble_size < 1) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: ensemble_size must be >= 1");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_noise_strength.
 */
  tmp_noise_strength = coerce_input_double(noise_strength,type_noise_strength,1,0,NULL,NULL);
  if(tmp_noise_strength == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for coercing noise_strength array to double");
    return(NhlFATAL);
  }
  if (*tmp_noise_strength < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: noise_strength must be non-negative");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
 * The current ones that are recognized are:
 *
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 */
  if(*opt) {
    stack_entry = _NclGetArg(4, 6, DONT_CARE);
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
 *   "S_number"
 *   "num_siftings"
 *   "rng_seed"
 */
        while (attr_list != NULL) {
/* S_number */
          if (!strcmp(attr_list->attname, "S_number")) {
            type_S_number = attr_list->attvalue->multidval.data_type;
            if(type_S_number != NCL_byte  && type_S_number == NCL_ubyte &&
               type_S_number != NCL_short && type_S_number == NCL_ushort &&
               type_S_number != NCL_int   && type_S_number == NCL_uint &&
               type_S_number != NCL_long  && type_S_number == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: The 'S_number' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_S_number = coerce_input_uint(attr_list->attvalue->multidval.val,
                                               type_S_number,1,0,NULL,NULL);
              set_S_number = True;
            }
          }
/* num_siftings */
          if (!strcmp(attr_list->attname, "num_siftings")) {
            type_num_siftings = attr_list->attvalue->multidval.data_type;
            if(type_num_siftings != NCL_byte  && type_num_siftings == NCL_ubyte &&
               type_num_siftings != NCL_short && type_num_siftings == NCL_ushort &&
               type_num_siftings != NCL_int   && type_num_siftings == NCL_uint &&
               type_num_siftings != NCL_long  && type_num_siftings == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: The 'num_siftings' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_num_siftings = coerce_input_uint(attr_list->attvalue->multidval.val,
                                                   type_num_siftings,1,0,NULL,NULL);
              set_num_siftings = True;
            }
          }
/* rng_seed */
          if (!strcmp(attr_list->attname, "rng_seed")) {
            type_rng_seed = attr_list->attvalue->multidval.data_type;
            if(type_rng_seed != NCL_byte  && type_rng_seed == NCL_ubyte &&
               type_rng_seed != NCL_short && type_rng_seed == NCL_ushort &&
               type_rng_seed != NCL_int   && type_rng_seed == NCL_uint &&
               type_rng_seed != NCL_long  && type_rng_seed == NCL_ulong) {
              NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: The 'rng_seed' attribute must be an integral value");
              return(NhlFATAL);
            }
            else {
              tmp_rng_seed = coerce_input_ulong(attr_list->attvalue->multidval.val,
                                                type_rng_seed,1,0,NULL,NULL);
              set_rng_seed = True;
            }
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

/*
 * Set default values for various parameters, if not set by user.
 */
  if(!set_S_number) {
    tmp_S_number = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_S_number = def_S_number;
  }
  if(!set_num_siftings) {
    tmp_num_siftings = (unsigned int *)malloc(sizeof(unsigned int));
    *tmp_num_siftings = def_num_siftings;
  }
  if(!set_rng_seed) {
    tmp_rng_seed = (unsigned long *)malloc(sizeof(unsigned long));
    *tmp_rng_seed = def_rng_seed;
  }
  if(tmp_S_number == NULL || tmp_num_siftings == NULL || tmp_rng_seed == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for various input parameters");
    return(NhlFATAL);
  }

/*
 * Error check attribute parameter values.
 */
  if (*tmp_S_number < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: S_number must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings < 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: num_siftings must be non-negative");
    return(NhlFATAL);
  }
  if (*tmp_S_number == 0 && *tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: one of S_number or num_siftings must be positive");
    return(NhlFATAL);
  }
  if (*tmp_num_siftings == 0) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: if S-number !=0 and num_siftings=0, then routine might never finish if stuck in some obscure numerical corner case");
    return(NhlFATAL);
  }    

/*
 * Some of these are needed for various array sizes.
 */
  ntnpts    = ntim * npts;
  nimnt     = tmp_num_imfs * ntim;
  nimntnpts = tmp_num_imfs * ntnpts;

/*
 * Allocate space for input array no matter what, because it 
 * may not be contiguous in memory.
 */
  tmp_input = (double *)calloc(ntim,sizeof(double));
  if(tmp_input == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
  if(type_input != NCL_double) type_output = NCL_float;
  else                         type_output = NCL_double;

/*
 * Compute size of output dimensions.
 */
  ndims_output = ndims_input + 1;
  dsizes_output = (ng_size_t*)calloc(ndims_output,sizeof(ng_size_t));  
  if( dsizes_output == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  dsizes_output[0] = tmp_num_imfs;
  for(i=1; i<ndims_output; i++) dsizes_output[i] = dsizes_input[i-1];

/* 
 * Allocate space for output array.
 */  
  tmp_output = (double *)calloc(nimnt,sizeof(double));
  if(tmp_output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for temporary eemd output array");
    return(NhlFATAL);
  }
  if(type_output != NCL_double) output = (void *)calloc(nimntnpts, sizeof(float));
  else                          output = (void *)calloc(nimntnpts, sizeof(double));
  if(output == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: Unable to allocate memory for eemd output array");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost/rightmost dimensions and call the
 * C routine for each subsection of the  input arrays.
 */
  nrnt = total_nr * ntim;
  for(i = 0; i < total_nl; i++) {
    index_nrnt = i*nrnt;
    for(j = 0; j < total_nr; j++) {
      index_input = index_nrnt + j;

      /* Coerce ntim subsection of input to double */
      coerce_subset_input_double_step(input,tmp_input,index_input,total_nr,
                                      type_input,ntim,0,NULL,NULL);
      
      error_code = eemd(tmp_input, ntim, tmp_output, tmp_num_imfs,
                        *tmp_ensemble_size, *tmp_noise_strength, 
                        *tmp_S_number, *tmp_num_siftings, 
                        *tmp_rng_seed);
      if(error_code) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eemd: EEMD decomposition routine failed\n");
        return(NhlFATAL);
      }
      /* Coerce (num_imfs x ntim) subsection of output back to appropriate type.
         The C routine returns eemd as num_imfs x ntim (in memory), so we need
         to copy to the correction index locations in the output array. */
      for(k = 0; k < tmp_num_imfs; k++) {
        index_output  = (k*ntnpts) + index_input;
        coerce_output_float_or_double_step(output,&tmp_output[k*ntim],type_output,ntim,
                                           index_output,total_nr);
      }
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(tmp_input);
  NclFree(tmp_output);
  if(type_ensemble_size  != NCL_uint)                          NclFree(tmp_ensemble_size);
  if(type_noise_strength != NCL_double)                        NclFree(tmp_noise_strength);
  if(!set_S_number       || type_S_number       != NCL_uint)   NclFree(tmp_S_number);
  if(!set_num_siftings   || type_num_siftings   != NCL_uint)   NclFree(tmp_num_siftings);
  if(!set_rng_seed       || type_rng_seed       != NCL_ulong)  NclFree(tmp_rng_seed);

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

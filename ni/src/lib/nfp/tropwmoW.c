#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(stattropx,STATTROPX)(int *, int *, double *, double *,
                                         double *, double *, int *, double *,
                                         int *, double *, double *, double *);

NhlErrorTypes trop_wmo_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *p;
  double *tmp_p = NULL;
  int ndims_p;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_p;

/*
 * Argument # 1
 */
  void *t;
  double *tmp_t = NULL;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t;

/*
 * Argument # 2
 */
  int *punit;

/*
 * Argument # 3
 */
  logical *opt;

/*
 * Return variable
 */
  void *ptrop;
  int *itrop;
  double *tmp_ptrop = NULL;
  int ndims_ptrop;
  ng_size_t size_ptrop;
  ng_size_t *dsizes_ptrop;
  NclBasicDataTypes type_ptrop;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Various
 */
  ng_size_t i, nlev, nlevm, index_t;
  int ret, inlev, inlevm;
  double tmsg;
  double *lapsec = NULL;
  double *lapse, *phalf, *pmb;
  logical lapsec_set = False;
  NclBasicDataTypes type_lapsec;
            
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  p = (void*)NclGetArgValue(
           0,
           4,
           &ndims_p,
           dsizes_p,
           NULL,
           NULL,
           &type_p,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  nlev = dsizes_p[ndims_p-1];

  nlevm = nlev+1;
  if((nlev > INT_MAX) || (nlevm > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlev = (int) nlev;
  inlevm = (int) nlevm;

/*
 * Get argument # 1
 */
  t = (void*)NclGetArgValue(
           1,
           4,
           &ndims_t,
           dsizes_t,
           NULL,
           NULL,
           &type_t,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_p > 1 && ndims_p != ndims_t) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: p must either be one-dimensional or the same dimensionality as t");
    return(NhlFATAL);
  }
  if(ndims_p > 1) {
    for(i = 0; i < ndims_p; i++) {
      if(dsizes_t[i] != dsizes_p[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: p must either be one-dimensional or the same dimensionality as t");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_t[ndims_t-1] != nlev) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: The rightmost dimension of t must be of length nlev");
      return(NhlFATAL);
    }
  }

/*
 * Get argument # 2
 */
  punit = (int*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 3
 */
  opt = (logical*)NclGetArgValue(
           3,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/* 
 * Allocate space for output dimension sizes and set them.
 */
  if(ndims_t > 1) {
    ndims_ptrop = ndims_t-1;
  }
  else {
    ndims_ptrop = 1;
  }
  dsizes_ptrop = (ng_size_t*)calloc(ndims_ptrop,sizeof(ng_size_t));  
  if( dsizes_ptrop == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  size_ptrop = 1;
  if(ndims_t > 1) {
    for(i = 0; i < ndims_ptrop; i++) {
      size_ptrop *= dsizes_t[i];
      dsizes_ptrop[i] = dsizes_t[i];
    }
  }
  else {
    dsizes_ptrop[0] = 1;
  }

/*
 * Allocate space for tmp_p.
 */
  if(ndims_p == 1) {
    tmp_p = coerce_input_double(p,type_p,nlev,0,NULL,NULL);
  }
  else { 
    if(type_p != NCL_double) {
      tmp_p = (double *)calloc(nlev,sizeof(double));
      if(tmp_p == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
      }
    }
  }

/*
 * Allocate space for tmp_t.
 */
  if(type_t != NCL_double) {
    tmp_t = (double *)calloc(nlev,sizeof(double));
    if(tmp_t == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }

/*
 * Set type of output.
 */
  if(type_p == NCL_double || type_t == NCL_double) {
    type_ptrop = NCL_double;
  }
  else {
    type_ptrop = NCL_float;
  }

/* 
 * Allocate space for output arrays.
 */
  if(type_ptrop != NCL_double) {
    ptrop = (void *)calloc(size_ptrop, sizeof(float));
    tmp_ptrop = (double *)calloc(1,sizeof(double));
    if(tmp_ptrop == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    ptrop = (void *)calloc(size_ptrop, sizeof(double));
  }
  itrop = (int *)calloc(size_ptrop, sizeof(int));
  if(itrop == NULL || ptrop == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for work arrays.
 */
  lapse = (double *)calloc(nlevm,sizeof(double));
  phalf = (double *)calloc(nlevm,sizeof(double));
  pmb   = (double *)calloc(nlev,sizeof(double));
  if(lapse == NULL || phalf == NULL || pmb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trop_wmo: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
  
/* 
 * If "opt" is True, then check if any attributes have been set.
 *
 * Right now, only "lapsec" is a recognized attribute.
 */
  if(*opt) {
    stack_entry = _NclGetArg(3, 4, DONT_CARE);
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
 * Loop through attributes and check them.
 */
        while (attr_list != NULL) {
/*
 * Check for "lapsec".
 */
          if(!strcmp(attr_list->attname, "lapsec")) {
            type_lapsec = attr_list->attvalue->multidval.data_type;
            lapsec = coerce_input_double(attr_list->attvalue->multidval.val,
                                         type_lapsec,1,0,NULL,NULL);
            lapsec_set = True;
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

/*
 * Default lapsec to 2.0 if not set as an attribute.
 */
  if(!lapsec_set) {
    lapsec = (double *)calloc(1,sizeof(double));
    *lapsec = 2.0;
  }

/*
 * Set tmsg to -999 for now. Later we may use input missing values.
 */
  tmsg = -999.;

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_t = 0;

  for(i = 0; i < size_ptrop; i++) {
/*
 * Coerce subsection of p (tmp_p) to double if necessary.
 */
    if(ndims_p > 1) {
      if(type_p != NCL_double) {
        coerce_subset_input_double(p,tmp_p,index_t,type_p,nlev,0,NULL,NULL);
      }
      else {
        tmp_p = &((double*)p)[index_t];
      }
    }

/*
 * Coerce subsection of t (tmp_t) to double if necessary.
 */
    if(type_t != NCL_double) {
      coerce_subset_input_double(t,tmp_t,index_t,type_t,nlev,0,NULL,NULL);
    }
    else {
      tmp_t = &((double*)t)[index_t];
    }

/*
 * Point temporary output array to void output array if appropriate.
 */
    if(type_ptrop == NCL_double) tmp_ptrop = &((double*)ptrop)[i];

/*
 * Call the Fortran routine.
 */
    NGCALLF(stattropx,STATTROPX)(&inlev, &inlevm, tmp_p, tmp_t, &tmsg, lapsec,
				 punit, tmp_ptrop, &itrop[i], lapse, phalf,
				 pmb);

/*
 * Coerce output back to float if necessary.
 */
    if(type_ptrop == NCL_float) {
      coerce_output_float_only(ptrop,tmp_ptrop,1,i);
    }
    index_t += nlev;
  }

/*
 * Free unneeded memory.
 */
  if(!lapsec_set || 
     (lapsec_set && type_lapsec != NCL_double)) NclFree(lapsec);
  if(type_t != NCL_double) NclFree(tmp_t);
  if(type_ptrop != NCL_double) NclFree(tmp_ptrop);
  if(type_p != NCL_double) NclFree(tmp_p);

  NclFree(itrop);
  NclFree(lapse);
  NclFree(phalf);
  NclFree(pmb);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(ptrop,ndims_ptrop,dsizes_ptrop,NULL,type_ptrop,0);
  NclFree(dsizes_ptrop);
  return(ret);
}

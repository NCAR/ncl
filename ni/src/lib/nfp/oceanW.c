#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dpotmp,DPOTMP)(double *,double *,double *,double *,
                                   double *);
extern void NGCALLF(dpth2pres,DPTH2PRES)(int *, double *, int *, double *, double *);


NhlErrorTypes depth_to_pres_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *z;
  double *tmp_z;
  int ndims_z;
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  int has_missing_z;
  NclScalar missing_z, missing_dbl_z, missing_flt_z;
  NclBasicDataTypes type_z;

/*
 * Argument # 1
 */
  logical *opt;

/*
 * Return variable
 */
  void *pres;
  double *tmp_pres = NULL;
  NclBasicDataTypes type_pres;
  NclScalar missing_pres;

/*
 * Various
 */
  ng_size_t i, nd;
  int ind, ret;
  double zmsg;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  z = (void*)NclGetArgValue(
           0,
           2,
           &ndims_z,
           dsizes_z,
           &missing_z,
           &has_missing_z,
           &type_z,
           DONT_CARE);

/*
 * Get argument # 1
 */
  opt = (logical*)NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  nd = 1;
  for(i = 0; i < ndims_z; i++) nd *= dsizes_z[i];

/*
 * Test input dimension sizes.
 */
  if(nd > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"depth_to_pres: the size of z is greater than INT_MAX");
    return(NhlFATAL);
  }
  ind = (int) nd;

/*
 * Coerce missing values to double if necessary.
 * Currently, missing values are not checked for.
 */
  coerce_missing(type_z,has_missing_z,&missing_z,&missing_dbl_z,
                 &missing_flt_z);

/*
 * The output type defaults to float, unless z is double.
 */
  if(type_z == NCL_double) type_pres = NCL_double;
  else                     type_pres = NCL_float;

  if(has_missing_z) {
    if(type_z == NCL_double) missing_pres = missing_dbl_z;
    else                     missing_pres = missing_flt_z;
    zmsg = missing_dbl_z.doubleval;
  }
  else {
    zmsg = 0.0;   /* Won't be used. */
  }

/* 
 * Coerce input array to double if necessary.
 */
  tmp_z = coerce_input_double(z,type_z,nd,0,NULL,NULL);
  if(tmp_z == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"depth_to_pres: Unable to allocate memory for coercing z to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_pres != NCL_double) {
    pres     = (void *)calloc(nd, sizeof(float));
    tmp_pres = (double *)calloc(nd,sizeof(double));
    if(tmp_pres == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"depth_to_pres: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    pres = (void *)calloc(nd, sizeof(double));
  }
  if(pres == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"depth_to_pres: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(type_pres == NCL_double) tmp_pres = &((double*)pres)[0];

/*
 * Call the Fortran routine.
 */
  NGCALLF(dpth2pres,DPTH2PRES)(&ind, tmp_z, &has_missing_z, &zmsg, tmp_pres);

/*
 * Coerce output back to float if necessary.
 */
  if(type_pres == NCL_float) coerce_output_float_only(pres,tmp_pres,nd,0);

/*
 * Free unneeded memory.
 */
  if(type_z    != NCL_double) NclFree(tmp_z);
  if(type_pres != NCL_double) NclFree(tmp_pres);

/*
 * Return value back to NCL script.
 */
  if(has_missing_z) {
    ret = NclReturnValue(pres,ndims_z,dsizes_z,&missing_pres,type_pres,0);
  }
  else {
    ret = NclReturnValue(pres,ndims_z,dsizes_z,NULL,type_pres,0);
  }
  return(ret);
}


NhlErrorTypes potmp_insitu_ocn_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *t;
  double *tmp_t;
  int ndims_t;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_t;
  int has_missing_t;
  NclScalar missing_t, missing_flt_t, missing_dbl_t;

/*
 * Argument # 1
 */
  void *s;
  double *tmp_s;
  int ndims_s;
  ng_size_t dsizes_s[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_s;
  int has_missing_s;
  NclScalar missing_s, missing_flt_s, missing_dbl_s;

/*
 * Argument # 2
 */
  void *pres;
  double *tmp_pres;
  int ndims_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pres;
  int is_scalar_pres;

/*
 * Argument # 3
 */
  void *pref;
  double *tmp_pref;
  NclBasicDataTypes type_pref;

/*
 * Argument # 4
 */
  int *dims;
  ng_size_t dsizes_dims[1];

/*
 * Argument # 5
 */
  logical *opt;
  logical reverse = False;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Return variable
 */
  void *pot;
  double *tmp_pot;
  NclBasicDataTypes type_pot;
  NclScalar missing_pot;

/*
 * Various
 */
  ng_size_t i, total_nts, total_npres, total_nl, total_nr, nrnpres;
  ng_size_t ipres;
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
  t = (void*)NclGetArgValue(
           0,
           6,
           &ndims_t,
           dsizes_t,
           &missing_t,
           &has_missing_t,
           &type_t,
           DONT_CARE);


/*
 * Get argument # 1
 */
  s = (void*)NclGetArgValue(
           1,
           6,
           &ndims_s,
           dsizes_s,
           &missing_s,
           &has_missing_s,
           &type_s,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_t != ndims_s) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: the dimensions of t and s must be the same");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_t; i++) {
    if(dsizes_t[i] != dsizes_s[i]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: the dimensions of t and s must be the same");
      return(NhlFATAL);
    }
  }

/*
 * Get argument # 2
 */
  pres = (void*)NclGetArgValue(
           2,
           6,
           &ndims_pres,
           dsizes_pres,
           NULL,
           NULL,
           &type_pres,
           DONT_CARE);

/*
 * Check dimension sizes and get total # of elements.
 */
  if(ndims_pres > ndims_t) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: the rank of pres must be less than or equal to the rank of t and s");
    return(NhlFATAL);
  }

/* Scalar pressure is a special case */
  is_scalar_pres = is_scalar(ndims_pres,dsizes_pres);

/*
 * Get argument # 3
 */
  pref = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_pref,
           DONT_CARE);

/*
 * Get argument # 4
 */
  dims = (int*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_dims,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Get argument # 5
 */
  opt = (logical*)NclGetArgValue(
           5,
           6,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Some error checking. Make sure pressure dimensions are valid.
 */

  if(!is_scalar_pres) {
    if(dsizes_dims[0] != ndims_pres) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: invalid number of dimension indexes given for 'pres'");
      return(NhlFATAL);
    }
    for(i = 0; i < dsizes_dims[0]; i++ ) {
      if(dims[i] < 0 || dims[i] >= ndims_t) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: invalid dimension indexes given for 'pres'");
        return(NhlFATAL);
      }
      if(i > 0 && dims[i] != (dims[i-1]+1)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: input dimension sizes must be monotonically increasing, can't continue");
        return(NhlFATAL);
      }
      if(dsizes_pres[i] != dsizes_t[dims[i]]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: dimension indexes given for 'pres' don't match dimensions of t and s");
        return(NhlFATAL);
      }
    }
  }


/*
 * Coerce missing values to double if necessary.
 */
  coerce_missing(type_t,has_missing_t,&missing_t,&missing_dbl_t,
                 &missing_flt_t);
  coerce_missing(type_s,has_missing_s,&missing_s,&missing_dbl_s,
                 &missing_flt_s);

/*
 * Compute the total number of leftmost and rightmost elements
 * in t and s.
 */
  if(is_scalar_pres) {
    total_nl = 1;
    for(i = 0; i < ndims_t; i++) total_nl *= dsizes_t[i];
    total_npres = nrnpres = total_nr = 1;
    total_nts  = total_nl;
  }
  else {
    total_npres = total_nl = total_nr = 1;
    for(i = 0; i < dims[0]; i++) total_nl *= dsizes_t[i];
    for(i = 0; i < ndims_pres; i++) total_npres *= dsizes_pres[i];
    for(i = dims[dsizes_dims[0]-1]+1; i < ndims_t; i++) total_nr *= dsizes_t[i];

    nrnpres    = total_nr * total_npres;
    total_nts  = total_nl * nrnpres;
  }
/*
 * The output type defaults to float, unless t is double.
 */
  if(type_t == NCL_double || type_s == NCL_double || 
     type_pres == NCL_double || type_pref == NCL_double) {
    type_pot = NCL_double;
    if(has_missing_t) {
      missing_pot = missing_dbl_t;
    }
    else if(has_missing_s) {
      missing_pot = missing_dbl_s;
    }
  }
  else {
    type_pot = NCL_float;
    if(has_missing_t) {
      missing_pot = missing_flt_t;
    }
    else if(has_missing_s) {
      missing_pot = missing_flt_s;
    }
  }

/* 
 * Coerce input arrays to double if necessary.
 */
  tmp_t    = coerce_input_double(t,   type_t,   total_nts,0,NULL,NULL);
  tmp_s    = coerce_input_double(s,   type_s,   total_nts,0,NULL,NULL);
  tmp_pres = coerce_input_double(pres,type_pres,total_npres,0,NULL,NULL);
  tmp_pref = coerce_input_double(pref,type_pref, 1,0,NULL,NULL);
  if(tmp_t == NULL || tmp_s == NULL || tmp_pres == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: Unable to allocate memory for coercing input arrays to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_pot != NCL_double) {
    pot = (void *)calloc(total_nts, sizeof(float));
    tmp_pot = (double*)calloc(1,sizeof(double));
  }
  else {
    pot = (void *)calloc(total_nts, sizeof(double));
  }
  if(pot == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"potmp_insitu_ocn: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
  if(*opt) {
    stack_entry = _NclGetArg(5, 6, DONT_CARE);
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
 * Loop through attributes and check them. The current ones recognized are:
 *   "reverse"
 */
        while (attr_list != NULL) {
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "reverse")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              reverse = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"potmp_insitu_ocn: The 'reverse' attribute must be a logical. Defaulting to False.");
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
 * Call the Fortran routine.
 */
  for(i = 0; i < total_nts; i++) {
    if(type_pot == NCL_double) tmp_pot = &((double*)pot)[i];

/* Calculate index into pressure array */
    ipres = (ng_size_t)((i-((ng_size_t)(i/nrnpres)*nrnpres))/total_nr);

    if(has_missing_t && tmp_t[i] == missing_dbl_t.doubleval) {
      *tmp_pot = missing_dbl_t.doubleval;
    }
    else if(has_missing_s && tmp_s[i] == missing_dbl_s.doubleval) {
      *tmp_pot = missing_dbl_s.doubleval;
    }
    else {
      if(reverse) {
        NGCALLF(dpotmp,DPOTMP)(tmp_pref, &tmp_t[i], &tmp_s[i],
                               &tmp_pres[ipres], tmp_pot);
      }
      else {
        NGCALLF(dpotmp,DPOTMP)(&tmp_pres[ipres], &tmp_t[i], &tmp_s[i],
                               tmp_pref, tmp_pot);
      }
    }
/*
 * Coerce output back to float if necessary.
 */
    if(type_pot == NCL_float) coerce_output_float_only(pot,tmp_pot,1,i);
  }

/*
 * Free unneeded memory.
 */
  if(type_t    != NCL_double) NclFree(tmp_t);
  if(type_s    != NCL_double) NclFree(tmp_s);
  if(type_pres != NCL_double) NclFree(tmp_pres);
  if(type_pref != NCL_double) NclFree(tmp_pref);
  if(type_pot  != NCL_double) NclFree(tmp_pot);

/*
 * Return value back to NCL script.
 */
  if(has_missing_t || has_missing_s) {
    ret = NclReturnValue(pot,ndims_t,dsizes_t,&missing_pot,type_pot,0);
  }
  else {
    ret = NclReturnValue(pot,ndims_t,dsizes_t,NULL,type_pot,0);
  }
  return(ret);
}


#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dpotmp,DPOTMP)(double *,double *,double *,double *,
                                   double *);
extern void NGCALLF(dpth2pres,DPTH2PRES)(int *, double *, int *, double *, double *);

extern void NGCALLF(mixed_layer_depth,MIXED_LAYER_DEPTH)(double *, int *, double *, double *, double *, int *, int *, int *, double *, double *);

extern void NGCALLF(wgt_area_smooth,WGT_AREA_SMOOTH)(double *, double *, double *, int *, int *, int *, double *, int *);

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

NhlErrorTypes wgt_area_smooth_W (void)
{
/*
 * Input variables
 */
/*
 * Argument # 0
 */
	void *field;
	double *tmp_field;
	int ndims_field;
	ng_size_t dsizes_field[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type_field;
	int has_missing_field;
	NclScalar missing_field, missing_flt_field, missing_dbl_field;

/*
 * Argument # 1
 */
	void *area;
	double *tmp_area;
	int ndims_area;
	ng_size_t dsizes_area[NCL_MAX_DIMENSIONS];
	NclBasicDataTypes type_area;
	int has_missing_area;
	NclScalar missing_area;

/*
 * Argument # 2
 */

  logical *opt;
  int cyclic = 1;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;


/*
 * Return variable
 */
	void *smooth_ret;
	double *tmp_smooth;
	NclBasicDataTypes type_smooth;
	NclScalar missing_smooth;

/*
 * Various
 */
	ng_size_t i, size_other,total_size,area_size;
	int dims[3];
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
	field = (void*)NclGetArgValue(
		0,
		3,
		&ndims_field,
		dsizes_field,
		&missing_field,
		&has_missing_field,
		&type_field,
		DONT_CARE);


/*
 * Get argument # 1
 */
	area = (void*)NclGetArgValue(
		1,
		3,
		&ndims_area,
		dsizes_area,
		&missing_area,
		&has_missing_area,
		&type_area,
		DONT_CARE);

/*
 * Get argument # 2
 */
	opt = (logical*)NclGetArgValue(
		2,
		3,
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
 *   "cyclic"
 */
				while (attr_list != NULL) {
/*
 * Check for "cyclic".
 */
					if (!strcmp(attr_list->attname, "cyclic")) {
						if(attr_list->attvalue->multidval.data_type == NCL_logical) {
							cyclic = *(logical*) attr_list->attvalue->multidval.val == False ? 0 : 1;
						}
						else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "wgt_area_smooth: The 'cyclic' attribute must be a logical. Defaulting to True.");
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
 * Check dimension sizes.
 */
	if(ndims_field <  ndims_area) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_area_smooth: field must have at least 2 dimensions");
		return(NhlFATAL);
	}
	if (dsizes_field[ndims_field - 2] != dsizes_area[0] ||
	    dsizes_field[ndims_field - 1] != dsizes_area[1]) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_area_smooth: the last two dimensions of field must be the same size as area");
		return(NhlFATAL);
	}
/* 
 * compute total elements in remaining dimensions of field
 */
	size_other = 1;
        total_size = 1;
	area_size = 1;
	for (i = 0; i < ndims_field - 2; i++) {
		size_other *= dsizes_field[i];
		total_size *= dsizes_field[i];
	}
	for (i = ndims_field - 2; i < ndims_field; i++) {
		total_size *= dsizes_field[i];
		area_size *= dsizes_field[i];
	}
	dims[0] = (int) dsizes_area[1];
	dims[1] = (int) dsizes_area[0];
	dims[2] = (int)size_other;


/*
 * Coerce missing values to double if necessary.
 */
	coerce_missing(type_field,has_missing_field,&missing_field,&missing_dbl_field,
		       &missing_flt_field);

/*
 * The output type defaults to float, unless t is double.
 */
	if(type_field == NCL_double) {
		type_smooth = NCL_double;
		if(has_missing_field) {
			missing_smooth = missing_dbl_field;
		}
	}
	else {
		type_smooth = NCL_float;
		if(has_missing_field) {
			missing_smooth = missing_flt_field;
		}
	}
/* 
 * Coerce input arrays to double if necessary.
 */
	tmp_field  = coerce_input_double(field,  type_field, total_size,0,NULL,NULL);
	tmp_area   = coerce_input_double(area,   type_area,   area_size,0,NULL,NULL);
	if(tmp_field == NULL || tmp_area == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_area_smooth: Unable to allocate memory for coercing input arrays to double");
		return(NhlFATAL);
	}
/* 
 * Allocate space for output array.
 */
	tmp_smooth = (void *)calloc(total_size, sizeof(double));
	if(tmp_smooth == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"wgt_area_smooth: Unable to allocate memory for output array");
		return(NhlFATAL);
	}

	NGCALLF(wgt_area_smooth,WGT_AREA_SMOOTH)(tmp_field,tmp_area,tmp_smooth,
						 &(dims[0]),&(dims[1]),&(dims[2]),&(missing_dbl_field.doubleval),&cyclic);
	if (type_smooth == NCL_float) {
		smooth_ret = (void *) coerce_output_float(tmp_smooth,NULL,total_size,0);
	}
	else {
		smooth_ret = (void *) tmp_smooth;
	}
	
	if (type_field == NCL_float) {
		NclFree(tmp_field);
	}
	if (type_area == NCL_float) {
		NclFree(tmp_area);
	}
	if(has_missing_field) {
		ret = NclReturnValue(smooth_ret,ndims_field,dsizes_field,&missing_smooth,type_smooth,0);
	}
	else {
		ret = NclReturnValue(smooth_ret,ndims_field,dsizes_field,NULL,type_smooth,0);
	}
	return(ret);

}
NhlErrorTypes mixed_layer_depth_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *pot_density;
  double *tmp_pot_density;
  int ndims_pot_density; 
  ng_size_t dsizes_pot_density[NCL_MAX_DIMENSIONS];
  int has_missing_pot_density;
  NclScalar missing_pot_density, missing_flt_pot_density, missing_dbl_pot_density;
  NclBasicDataTypes type_pot_density;

/*
 * Argument # 1
 */
  int *kmt;
  ng_size_t dsizes_kmt[2];
/*
 * Argument # 2
 */
  void *ht;
  double *tmp_ht;
  ng_size_t dsizes_ht[2];
  NclBasicDataTypes type_ht;

/*
 * Argument # 3
 */
  void *depth;
  double *tmp_depth;
  ng_size_t dsizes_depth[1];
  NclBasicDataTypes type_depth;

/*
 * Argument # 4
 */
  void *offset;
  double *tmp_offset;
  NclBasicDataTypes type_offset;

/*
 * Return variable
 */
  void *mld;
  double *tmp_mld;
  int ndims_mld; 
  ng_size_t *dsizes_mld;
  ng_size_t index_mld;
  NclScalar missing_mld, missing_flt_mld, missing_dbl_mld;
  NclBasicDataTypes type_mld;


/*
 * Various
 */
  int nz, ny, nx, nznynx, nynx ;
  int index_pot_density;
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
  pot_density = (void*)NclGetArgValue(
           0,
           5,
           &ndims_pot_density,
           dsizes_pot_density,
           &missing_pot_density,
           &has_missing_pot_density,
           &type_pot_density,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_pot_density < 3) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The pot_density array must have at least 3 dimensions");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_pot_density,has_missing_pot_density,&missing_pot_density,
                 &missing_dbl_pot_density,&missing_flt_pot_density);

  nz = (int)dsizes_pot_density[ndims_pot_density-3];
  ny = (int)dsizes_pot_density[ndims_pot_density-2];
  nx = (int)dsizes_pot_density[ndims_pot_density-1];
  nznynx = nz * ny * nx;

/*
 * Get argument # 1
 */
  kmt = (int*)NclGetArgValue(
           1,
           5,
           NULL,
           dsizes_kmt,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
  if(dsizes_kmt[0] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The #0 dimension of kmt must be length ny");
    return(NhlFATAL);
  }
  if(dsizes_kmt[1] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The #1 dimension of kmt must be length nx");
    return(NhlFATAL);
  }
  nynx = ny * nx;

/*
 * Get argument # 2
 */
  ht = (void*)NclGetArgValue(
           2,
           5,
           NULL,
           dsizes_ht,
           NULL,
           NULL,
           &type_ht,
           DONT_CARE);
  if(dsizes_ht[0] != ny) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The #0 dimension of ht must be length ny");
    return(NhlFATAL);
  }
  if(dsizes_ht[1] != nx) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The #1 dimension of ht must be length nx");
    return(NhlFATAL);
  }
  nynx = ny * nx;

/*
 * Get argument # 3
 */
  depth = (void*)NclGetArgValue(
           3,
           5,
           NULL,
           dsizes_depth,
           NULL,
           NULL,
           &type_depth,
           DONT_CARE);
  if(dsizes_depth[0] != nz) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: The #0 dimension of depth must be length nz");
    return(NhlFATAL);
  }
/*
 * Get argument # 4
 */
  offset = (void*)NclGetArgValue(
           4,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           &type_offset,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions.
 */
  size_leftmost  = 1;
  ndims_leftmost = ndims_pot_density-3;
  for(i = 0; i < ndims_leftmost; i++) {
	  size_leftmost *= (int)dsizes_pot_density[i];
  }


/*
 * The output type defaults to float, unless this input array is double.
 */
  type_mld = NCL_float;

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_pot_density.
 */
  if(type_pot_density != NCL_double) {
    tmp_pot_density = (double *)calloc(nznynx,sizeof(double));
    if(tmp_pot_density == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for coercing input array to double");
      return(NhlFATAL);
    }
  }
  else {
    type_mld = NCL_double;
  }
/*
 * Allocate space for tmp_ht.
 */
  tmp_ht = coerce_input_double(ht,type_ht,nynx,0,NULL,NULL);
  if(tmp_ht == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_depth.
 */
  tmp_depth = coerce_input_double(depth,type_depth,nz,0,NULL,NULL);
  if(tmp_depth == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_offset.
 */
  tmp_offset = coerce_input_double(offset,type_offset,1,0,NULL,NULL);
  if(tmp_offset == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/*
 * Calculate size of output array.
 */
  nynx  = ny * nx;
  size_output = size_leftmost * nynx;

/* 
 * Allocate space for output array.
 */
  if(type_mld != NCL_double) {
    mld = (void *)calloc(size_output, sizeof(float));
    tmp_mld = (double *)calloc(nynx,sizeof(double));
    if(tmp_mld == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for temporary output array");
      return(NhlFATAL);
    }
  }
  else {
    mld = (void *)calloc(size_output, sizeof(double));
    tmp_mld = (double *)mld;
  }
  if(mld == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  if(has_missing_pot_density) {
    if(type_mld == NCL_double) missing_mld = missing_dbl_pot_density;
    else                 missing_mld = missing_flt_pot_density;
    missing_dbl_mld = missing_dbl_pot_density;
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_mld = ndims_leftmost + 2;
  dsizes_mld = (ng_size_t*)calloc(ndims_mld,sizeof(ng_size_t));  
  if( dsizes_mld == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"mixed_layer_depth: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_mld; i++) {
	  if (i < ndims_leftmost) {
		  dsizes_mld[i] = dsizes_pot_density[i];
	  }
	  else {
		  dsizes_mld[i] = dsizes_pot_density[i+1];
	  }
  }

/*
 * Loop across leftmost dimensions and call the Fortran routine for each
 * subsection of the input arrays.
 */
  index_pot_density = 0;
  index_mld = 0;

  for(i = 0; i < size_leftmost; i++) {
/*
 * Coerce subsection of pot_density (tmp_pot_density) to double if necessary.
 */
    if(type_pot_density != NCL_double) {
      coerce_subset_input_double(pot_density,tmp_pot_density,index_pot_density,type_pot_density,nznynx,0,NULL,NULL);
    }
    else {
      tmp_pot_density = &((double*)pot_density)[index_pot_density];
    }

/*
 * Call the Fortran routine.
 */
    NGCALLF(mixed_layer_depth,MIXED_LAYER_DEPTH)(tmp_pot_density, kmt, tmp_ht, tmp_depth, tmp_mld + index_mld, 
						 &nx, &ny, &nz, tmp_offset, &missing_dbl_pot_density.doubleval);

/*
 * Coerce output back to float if necessary.
 */
    if(type_mld == NCL_float) {
      coerce_output_float_only(mld,tmp_mld,nynx,index_mld);
    }
    index_pot_density += nznynx;
    index_mld += nynx;
  }

/*
 * Free unneeded memory.
 */
  if(type_pot_density != NCL_double) NclFree(tmp_pot_density);
  if(type_ht != NCL_double) NclFree(tmp_ht);
  if(type_depth != NCL_double) NclFree(tmp_depth);
  if(type_offset != NCL_double) NclFree(tmp_offset);
  if(type_mld != NCL_double) NclFree(tmp_mld);

/*
 * Return value back to NCL script.
 */
  if(type_mld != NCL_double) {
    ret = NclReturnValue(mld,ndims_mld,dsizes_mld,&missing_flt_mld,type_mld,0);
  }
  else {
    ret = NclReturnValue(mld,ndims_mld,dsizes_mld,&missing_dbl_mld,type_mld,0);
  }
  NclFree(dsizes_mld);
  return(ret);
}


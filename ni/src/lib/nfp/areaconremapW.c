#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(cremapbin,cremapbin)(int *, int *, int *, int *, int *,
                                         double *, double *, double *, 
                                         double *, double *, double *, int *,
                                         int *, double *, double *);

NhlErrorTypes area_conserve_remap_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *loni;
  double *tmp_loni;
  ng_size_t dsizes_loni[1];
  NclBasicDataTypes type_loni;

/*
 * Argument # 1
 */
  void *lati;
  double *tmp_lati;
  ng_size_t dsizes_lati[1];
  NclBasicDataTypes type_lati;

/*
 * Argument # 2
 */
  void *fi;
  double *tmp_fi = NULL;
  int ndims_fi;
  ng_size_t dsizes_fi[NCL_MAX_DIMENSIONS];
  int has_missing_fi;
  NclScalar missing_fi, missing_flt_fi, missing_dbl_fi;
  NclBasicDataTypes type_fi;

/*
 * Argument # 3
 */
  void *lono;
  double *tmp_lono;
  ng_size_t dsizes_lono[1];
  NclBasicDataTypes type_lono;

/*
 * Argument # 4
 */
  void *lato;
  double *tmp_lato;
  ng_size_t dsizes_lato[1];
  NclBasicDataTypes type_lato;

/*
 * Argument # 5
 */
  logical *opt;
/*
 * Return variable
 */
  void *fo;
  double *tmp_fo;
  ng_size_t *dsizes_fo;
  NclBasicDataTypes type_fo;


/*
 * Various
 */
  ng_size_t nloni, nlati, nlevi, nlono, nlato, nlevnlatnloni, nlevnlatnlono;
  ng_size_t NLATi, NLATo, i;
  int ret;
  double *bin_factor = NULL;
  logical set_binf;
  NclBasicDataTypes type_bin_factor;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Variables for coercing input dimension sizes to integer.
 */
  int inlono, inlato, iNLATo, iNLATi, inloni, inlati, inlevi;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  loni = (void*)NclGetArgValue(
           0,
           6,
           NULL,
           dsizes_loni,
           NULL,
           NULL,
           &type_loni,
           DONT_CARE);
  nloni = dsizes_loni[0];
/*
 * Get argument # 1
 */
  lati = (void*)NclGetArgValue(
           1,
           6,
           NULL,
           dsizes_lati,
           NULL,
           NULL,
           &type_lati,
           DONT_CARE);
  nlati = dsizes_lati[0];
/*
 * Get argument # 2
 */
  fi = (void*)NclGetArgValue(
           2,
           6,
           &ndims_fi,
           dsizes_fi,
           &missing_fi,
           &has_missing_fi,
           &type_fi,
           DONT_CARE);

/*
 * Check dimension sizes.
 */
  if(ndims_fi < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: The fi array must have at least 2 dimensions");
    return(NhlFATAL);
  }

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_fi,has_missing_fi,&missing_fi,
                 &missing_dbl_fi,&missing_flt_fi);

  if(dsizes_fi[ndims_fi-2] != nlati || dsizes_fi[ndims_fi-1] != nloni) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: The rightmost two dimension of fi must be nlat x nlon");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  lono = (void*)NclGetArgValue(
           3,
           6,
           NULL,
           dsizes_lono,
           NULL,
           NULL,
           &type_lono,
           DONT_CARE);
  nlono = dsizes_lono[0];
/*
 * Get argument # 4
 */
  lato = (void*)NclGetArgValue(
           4,
           6,
           NULL,
           dsizes_lato,
           NULL,
           NULL,
           &type_lato,
           DONT_CARE);
  nlato = dsizes_lato[0];
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
 * Check for the following attributes attached to "opt":
 *   NLATi
 *   NLATo
 *   bin_factor
 *
 * If not found, then use default values, which are set here.
 * "bin_factor" will be set later.
 */
  NLATi    = nlati;
  NLATo    = nlato;
  set_binf = False;

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
 * att_id == -1 ==> no attributes specified.
 */
        break;
      }
/* 
 * Check for attributes. If none are set, then use default values.
 */
      if (attr_obj->att.n_atts == 0) {
        break;
      }
      else {
/*
 * Get list of attributes.
 */
        attr_list = attr_obj->att.att_list;
/*
 * Loop through attributes and check them.
 */
        while (attr_list != NULL) {
/*
 * NLATi
 */
          if ((strcmp(attr_list->attname, "NLATi")) == 0) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"area_conserve_remap: The 'NLATi' attribute must be an integer, defaulting to nlati.");
            }
            else {
              NLATi = *(int*) attr_list->attvalue->multidval.val;
            }
          }
/*
 * NLATo
 */
          if ((strcmp(attr_list->attname, "NLATo")) == 0) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"area_conserve_remap: The 'Nlato' attribute must be an integer, defaulting to nlato.");
            }
            else {
              NLATo = *(int*) attr_list->attvalue->multidval.val;
            }
          }
/*
 * bin_factor
 */
          if(!strcmp(attr_list->attname, "bin_factor")) {
            type_bin_factor = attr_list->attvalue->multidval.data_type;
            bin_factor = coerce_input_double(attr_list->attvalue->multidval.val,
                                             type_bin_factor,1,0,NULL,NULL);
            set_binf = True;
          }
          attr_list = attr_list->next;
        }
      }
    default:
      break;
    }
  }

  if(!set_binf) {
    bin_factor = (double *)calloc(1,sizeof(double));
    *bin_factor = 1.0;
  }
/*
 * Calculate size of leftmost dimensions and fi/fo.
 */
  nlevi = 1;
  for(i = 0; i < ndims_fi-2; i++) nlevi *= dsizes_fi[i];

/*
 * Test input dimension sizes to make sure they are <= INT_MAX.
 */
  if((nlono > INT_MAX) ||
     (nlato > INT_MAX) ||
     (NLATi > INT_MAX) ||
     (NLATo > INT_MAX) ||
     (nloni > INT_MAX) ||
     (nlati > INT_MAX) ||
     (nlevi > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: One of the input array dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlono = (int) nlono;
  inlato = (int) nlato;
  iNLATo = (int) NLATo;
  iNLATi = (int) NLATi;
  inloni = (int) nloni;
  inlati = (int) nlati;
  inlevi = (int) nlevi;

  nlevnlatnloni = nlevi * nlati * nloni;   /* input array size */
  nlevnlatnlono = nlevi * nlato * nlono;   /* output array size */

/* 
 * Allocate space for coercing input arrays.  If any of the input
 * is already double, then we don't need to allocate space for
 * temporary arrays, because we'll just change the pointer into
 * the void array appropriately.
 */
/*
 * Allocate space for tmp_loni.
 */
  tmp_loni = coerce_input_double(loni,type_loni,nloni,0,NULL,NULL);
  if(tmp_loni == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_lati.
 */
  tmp_lati = coerce_input_double(lati,type_lati,nlati,0,NULL,NULL);
  if(tmp_lati == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_fi and determine type of output.
 *
 * The output type defaults to float, unless fi is double.
 */
  if(type_fi != NCL_double) {
    type_fo = NCL_float;
  }
  else {
    type_fo = NCL_double;
  }
/*
 * Coerce input to double if necessary.
 */
  tmp_fi = coerce_input_double(fi,type_fi,nlevnlatnloni,0,NULL,NULL);
  if(tmp_fi == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for coercing fi to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_lono.
 */
  tmp_lono = coerce_input_double(lono,type_lono,nlono,0,NULL,NULL);
  if(tmp_lono == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for coercing lono to double");
    return(NhlFATAL);
  }
/*
 * Allocate space for tmp_lato.
 */
  tmp_lato = coerce_input_double(lato,type_lato,nlato,0,NULL,NULL);
  if(tmp_lato == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for coercing lato to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_fo != NCL_double) {
    fo     = (void *)calloc(nlevnlatnlono, sizeof(float));
    tmp_fo = (double *)calloc(nlevnlatnlono,sizeof(double));
    if(fo == NULL || tmp_fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    fo     = (void *)calloc(nlevnlatnlono, sizeof(double));
    if(fo == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_fo = fo;
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  dsizes_fo = (ng_size_t*)calloc(ndims_fi,sizeof(ng_size_t));  
  if( dsizes_fo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"area_conserve_remap: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_fi-2; i++) dsizes_fo[i] = dsizes_fi[i];
  dsizes_fo[ndims_fi-2] = nlato;
  dsizes_fo[ndims_fi-1] = nlono;

/*
 * Call the Fortran routine.
 */
  NGCALLF(cremapbin,CREMAPBIN)(&inlevi, &inlato, &inlono, &inlati, &inloni, 
                               tmp_fi, tmp_fo, tmp_lati, tmp_loni, tmp_lato,
                               tmp_lono, &iNLATi, &iNLATo, bin_factor, 
                               &missing_dbl_fi.doubleval);
  if (!set_binf || (set_binf && type_bin_factor != NCL_double)) {
          free(bin_factor);
  }

/*
 * Coerce output back to float if necessary.
 */
  if(type_fo == NCL_float) {
    coerce_output_float_only(fo,tmp_fo,nlevnlatnlono,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_loni != NCL_double) NclFree(tmp_loni);
  if(type_lati != NCL_double) NclFree(tmp_lati);
  if(type_fi   != NCL_double) NclFree(tmp_fi);
  if(type_lono != NCL_double) NclFree(tmp_lono);
  if(type_lato != NCL_double) NclFree(tmp_lato);
  if(type_fo   != NCL_double) NclFree(tmp_fo);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(fo,ndims_fi,dsizes_fo,NULL,type_fo,0);
  NclFree(dsizes_fo); 
  return(ret);
}


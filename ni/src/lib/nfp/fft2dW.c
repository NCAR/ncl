#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(drfft2i,DRFFT2I)(int *, int *, double *, int *, int *);
extern void NGCALLF(drfft2f,DRFFT2F)(int *, int *, int *, double *, double *,
                                     int *, double *, int *, int *);
extern void NGCALLF(drfft2b,DRFFT2B)(int *, int *, int *, double *, double *,
                                     int *, double *, int *, int *);

NhlErrorTypes fft2df_W( void )
{
/*
 * First and only input argument.
 */
  void *x;
  double *tmp_x, *tmp_r;
  ng_size_t dsizes_x[2];
  NclBasicDataTypes type_x;

/*
 * Return variable
 */
  void *coef;
  ng_size_t dsizes_coef[3];
  NclBasicDataTypes type_coef;
  NclObjClass type_obj_coef;
/*
 * Variables for returning the output array with attributes attached.
 */
  int att_id;
  ng_size_t dsizes[1];
  int *nattr, *mattr;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Various
 */
  int ier;
  ng_size_t i, j, m, l, ldim, l21, ml, mldim, ml21, lwsave, lwork, size_coef;
  ng_size_t ic0, ic1, ir0, ir1, ix0, ix1;
  double *wsave, *work;
  int il, im, ildim, ilwsave, ilwork;

/*
 * Retrieve input argument.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  x = (void*)NclGetArgValue(
           0,
           1,
           NULL,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);
/* 
 * Allocate space for coercing input array. Since we have to copy
 * the input array to a bigger array (tmp_r), we will go ahead and
 * make a copy of it.
 */
  m     = dsizes_x[0];
  l     = dsizes_x[1];
  l21   = (l/2) + 1;
  ldim  = 2 * l21;
  ml    = m * l;
  ml21  = m * l21;
  mldim = m * ldim;

  tmp_x = (double *)calloc(ml, sizeof(double));
  tmp_r = (double *)calloc(mldim, sizeof(double));

  if(tmp_x == NULL || tmp_r == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2df: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  coerce_subset_input_double(x,tmp_x,0,type_x,ml,0,NULL,NULL);

/*
 * Copy tmp_x to a subset of tmp_r.
 */
  for(i = 0; i < m; i++ ) {
    for(j = 0; j < l; j++ ) {
      ix0 = i*l + j;
      ix1 = i*ldim + j;
      tmp_r[ix1] = tmp_x[ix0];
    }
  }

/*
 * The output type defaults to float, unless the input array is double.
 */
  if(type_x != NCL_double) {
    type_coef =     NCL_float;
    type_obj_coef = nclTypefloatClass;
  }
  else {
    type_coef     = NCL_double;
    type_obj_coef = nclTypedoubleClass;
  }

/*
 * Calculate size of output array and allocate space for it.
 */
  dsizes_coef[0] = 2;
  dsizes_coef[1] = m;
  dsizes_coef[2] = l21;
  size_coef      = 2 * ml21;

  if(type_coef != NCL_double) {
    coef = (void *)calloc(size_coef, sizeof(float));
  }
  else {
    coef = (void *)calloc(size_coef, sizeof(double));
  }
  if(coef == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2df: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes. 
 */
  lwsave = 2*m + l + (int)log((double)l) + (int)log((double)m) + 8;
  lwork  = mldim;
  if((l > INT_MAX) || (m > INT_MAX) || (ldim > INT_MAX) ||
     (lwsave > INT_MAX) || (lwork > INT_MAX)) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2df: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  il = (int) l;
  im = (int) m;
  ildim = (int) ldim;
  ilwsave = (int) lwsave;
  ilwork = (int) lwork;

/*
 * Allocate space for work arrays.
 */
  wsave  = (double *)calloc(lwsave,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  if(work == NULL || wsave == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2df: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routines.
 */
  ier = 0;
  NGCALLF(drfft2i,DRFFT2I)(&il, &im, wsave, &ilwsave, &ier);
  NGCALLF(drfft2f,DRFFT2F)(&ildim, &il, &im, tmp_r, wsave, &ilwsave, work, &ilwork,
                             &ier);
  if(ier) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2df: ier = %d", ier);
    return(NhlFATAL);
  }
/*
 * Copy tmp_r back to the appropriate locations in coef.
 */
  if(type_coef == NCL_float) {
    for(i = 0; i < m; i++ ) {
      for(j = 0; j < l21; j++ ) {
        ic0 = i*l21 + j;
        ic1 = ml21 + ic0;
        ir0 = i*ldim + 2*j;
        ir1 = ir0 + 1;
        ((float*)coef)[ic0] = (float)tmp_r[ir0];
        ((float*)coef)[ic1] = (float)tmp_r[ir1];
      }
    }
  }
  else {
    for(i = 0; i < m; i++ ) {
      for(j = 0; j < l21; j++ ) {
        ic0 = i*l21 + j;
        ic1 = ml21 + ic0;
        ir0 = i*ldim + 2*j;
        ir1 = ir0 + 1;
        ((double*)coef)[ic0] = tmp_r[ir0];
        ((double*)coef)[ic1] = tmp_r[ir1];
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(tmp_r);
  NclFree(wsave);
  NclFree(work);

/*
 * Set up return value.
 */
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            (void*)coef,
                            NULL,
                            3,
                            dsizes_coef,
                            TEMPORARY,
                            NULL,
                            type_obj_coef
                            );
/*
 * Set up attributes to return.
 */
  nattr = (int *)malloc(sizeof(int));
  mattr = (int *)malloc(sizeof(int));
  *nattr = l;
  *mattr = m; 

  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)nattr,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "N",
             att_md,
             NULL
             );
    
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)mattr,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "M",
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
/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);

}


NhlErrorTypes fft2db_W( void )
{
/*
 * First and only input argument.
 */
  void *coef;
  double *tmp_coef, *tmp_r;
  ng_size_t dsizes_coef[3];
  NclBasicDataTypes type_coef;

/*
 * Return variable
 */
  void *x;
  ng_size_t dsizes_x[2];
  NclBasicDataTypes type_x;
  NclObjClass type_obj_x;

/*
 * Variables for retrieving attributes from input array.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Various
 */
  int ier;
  ng_size_t l = 0;
  ng_size_t i, j, m, ldim, l21, ml, mldim, ml21, lwsave, lwork;
  ng_size_t ix0, ix1, ir0, ir1, ic0, ic1, size_coef;
  logical calculate_lval;
  double *wsave, *work;
  int il, im, ildim, ilwsave, ilwork;

/*
 * Retrieve input argument.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
  coef = (void*)NclGetArgValue(
           0,
           1,
           NULL,
           dsizes_coef,
           NULL,
           NULL,
           &type_coef,
           DONT_CARE);
/*
 * First dimension must be 2.
 */
  if(dsizes_coef[0] != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: The first dimension of coef must be 2");
    return(NhlFATAL);
  }

/*
 * Check if the N attribute is attached to the input array (there should
 * also be an M attribute, but we don't need this).
 *
 * This will tell us the original size of the array passed to "fft2df".
 * If N is not attached as an attribute, then we will just calculate N
 * (which is "l" in the code below).
 */
  calculate_lval = True;
  stack_entry = _NclGetArg(0, 1, DONT_CARE);
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
 * att_id == -1 ==> no optional args given; will have to calculate "l".
 */
      break;
    }
/* 
 * Look for attributes.
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
        if ((strcmp(attr_list->attname, "N")) == 0) {
          l = *(int *) attr_list->attvalue->multidval.val;
          if (l > 0) {
            calculate_lval = False;
          }
        }
        attr_list = attr_list->next;
      }
    }
  default:
    break;
  }

/* 
 * Allocate space for coercing input array. Since we have to copy
 * the input array to a bigger array (tmp_r), we will go ahead and
 * make a copy of it.
 */
  m         = dsizes_coef[1];
  l21       = dsizes_coef[2];
  if(calculate_lval) {
    l = (l21-1) * 2;
    NhlPError(NhlWARNING,NhlEUNKNOWN,"fft2db: 'N' was either not attached as an attribute to the input array, or it had an invalid value.\nThe size of the output array will have to be calculated based on the size of the input array.");
  }
 ldim      = 2 * l21;
  ml        = m * l;
  mldim     = m * ldim;
  ml21      = m * l21;
  size_coef = 2 * ml21;

  tmp_coef = (double *)calloc(size_coef, sizeof(double));
  tmp_r    = (double *)calloc(mldim, sizeof(double));

  if(tmp_coef == NULL || tmp_r == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

  coerce_subset_input_double(coef,tmp_coef,0,type_coef,size_coef,0,NULL,NULL);

/*
 * Copy tmp_coef array to a subset of tmp_r.
 */
    for(i = 0; i < m; i++ ) {
      for(j = 0; j < l21; j++ ) {
        ic0 = i*l21 + j;
        ic1 = ml21 + ic0;
        ir0 = i*ldim + 2*j;
        ir1 = ir0 + 1;
        tmp_r[ir0] = tmp_coef[ic0];
        tmp_r[ir1] = tmp_coef[ic1];
      }
  }

/*
 * The output type defaults to float, unless the input array is double.
 */
  if(type_coef != NCL_double) {
    type_x     = NCL_float;
    type_obj_x = nclTypefloatClass;
  }
  else {
    type_x     = NCL_double;
    type_obj_x = nclTypedoubleClass;
  }

/*
 * Calculate size of output array and allocate space for it.
 */
  dsizes_x[0] = m;
  dsizes_x[1] = l;

  if(type_x != NCL_double) {
    x = (void *)calloc(ml, sizeof(float));
  }
  else {
    x = (void *)calloc(ml, sizeof(double));
  }
  if(x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes. 
 */
  lwsave = 2*m + l + log(l) + log(m) + 8;
  lwork  = mldim;
  if((l > INT_MAX) || (m > INT_MAX) || (ldim > INT_MAX) ||
     (lwsave > INT_MAX) || (lwork > INT_MAX)) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: one or more input dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  il = (int) l;
  im = (int) m;
  ildim = (int) ldim;
  ilwsave = (int) lwsave;
  ilwork = (int) lwork;

/*
 * Allocate space for work arrays.
 */
  wsave  = (double *)calloc(lwsave,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  if(work == NULL || wsave == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/*
 * Call the Fortran routines.
 */
  ier = 0;
  NGCALLF(drfft2i,DRFFT2I)(&il, &im, wsave, &ilwsave, &ier);
  NGCALLF(drfft2b,DRFFT2B)(&ildim, &il, &im, tmp_r, wsave, &ilwsave, work, 
                           &ilwork,&ier);

  if(ier) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"fft2db: ier = %d", ier);
    return(NhlFATAL);
  }
/*
 * Copy tmp_r back to the appropriate locations in coef.
 */
  if(type_x == NCL_float) {
    for(i = 0; i < m; i++ ) {
      for(j = 0; j < l; j++ ) {
        ix0 = i*l + j;
        ix1 = i*ldim + j;
        ((float*)x)[ix0] = (float)tmp_r[ix1]; 
     }
    }
  }
  else {
    for(i = 0; i < m; i++ ) {
      for(j = 0; j < l; j++ ) {
        ix0 = i*l + j;
        ix1 = i*ldim + j;
        ((double*)x)[ix0] = tmp_r[ix1];
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_coef);
  NclFree(tmp_r);
  NclFree(wsave);
  NclFree(work);

/*
 * Return value back to NCL script.
 */
  return(NclReturnValue(x,2,dsizes_x,NULL,type_x,0));
}

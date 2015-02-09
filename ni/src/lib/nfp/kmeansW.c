#include <stdio.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(kmns136,KMNS136)(double *, int *, int *, double *, 
                                     int *, int *, int *, int *, int *,
                                     double *, int *);

NhlErrorTypes kmeans_as136_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int       ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *k;

/*
 * Argument # 2
 */
  logical *opt;

/*
 * Return variable
 */
  void *clstr;
  double *tmp_clstr;
  int ndims_clstr;
  ng_size_t *dsizes_clstr;
  NclBasicDataTypes type_clstr;
  NclTypeClass type_clstr_class;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  int iter=25, iseed=1;

/*
 * Variables for returning data with attributes.
 */
  int att_id, ndims_atts;
  ng_size_t dsizes_atts[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Various
 */
  ng_size_t i, ntime, nspatial;
  ng_size_t size_x, size_clstr;
  int *ic1, *nc;
  int intime, inspatial, ier;
  double *wss;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x,
           dsizes_x,
           NULL,
           NULL,
           &type_x,
           DONT_CARE);

  if(ndims_x < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: x must have at least two dimensions.");
    return(NhlFATAL);
  }
/*
 * Get argument # 1
 */
  k = (int*)NclGetArgValue(
           1,
           3,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
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
 * Array to dimension sizes for output.
 */
  ndims_clstr  = ndims_x;
  dsizes_clstr = (ng_size_t*)malloc(ndims_clstr*sizeof(ng_size_t));  
  if(dsizes_clstr == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

/*
 * Get size of spatial dimensions (all dimensions of x except time)
 * It is assumed that time is the righmost dimension of x.
 */
  nspatial = 1;
  dsizes_clstr[0] = *k;
  for( i = 0; i < ndims_x-1; i++ ) {
    nspatial *= dsizes_x[i];
    dsizes_clstr[i+1] = dsizes_x[i];
  }
/*
 * Test dimension sizes.
 */
  ntime = dsizes_x[ndims_x-1];
  if((ntime > INT_MAX) || (nspatial > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: # of observations and/or variables is greater than INT_MAX");
    return(NhlFATAL);
  }
  inspatial = (int) nspatial;
  intime    = (int) ntime;
  size_x    = nspatial * ntime; 
/*
 * Check for attributes attached to "opt"
 *
 *   "iter"   25
 *   "iseed"   1
 */
  if(*opt) {
    stack_entry = _NclGetArg(2, 3, DONT_CARE);
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
          if(!strcasecmp(attr_list->attname, "iter")) {
            iter = *(int *) attr_list->attvalue->multidval.val;
          }
          else if(!strcasecmp(attr_list->attname, "iseed")) {
            iseed = *(int *) attr_list->attvalue->multidval.val;
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
  }
  if(iseed != 1 && iseed != 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: iseed must be 1 or 2");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  size_clstr = nspatial * *k;
  if(type_x != NCL_double) {
    type_clstr = NCL_float;
    clstr      = (void*)calloc(size_clstr,sizeof(float));
    tmp_clstr  = (double *)calloc(size_clstr,sizeof(double));
    if(clstr == NULL || tmp_clstr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    type_clstr = NCL_double;
    clstr      = (void*)calloc(size_clstr,sizeof(double));
    if(clstr == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_clstr = &((double*)clstr)[0];
  }

/*
 * Allocate space for return attribute arrays.
 */
  nc  = (int*)malloc(*k*sizeof(int));
  ic1 = (int*)malloc(ntime*sizeof(int));
  wss = (double*)malloc(*k*sizeof(double));
  if(nc == NULL || ic1== NULL || wss == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: Unable to allocate memory for return attribute arrays.");
    return(NhlFATAL);
  }

/*
 * Loop across leftmost dimensions and call 
 * the Fortran routine for each subsection of the 
 * input array.
 */
  tmp_x = coerce_input_double(x,type_x,size_x,0,NULL,NULL);
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"kmeans_as136: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }


  NGCALLF(kmns136,KMNS136)(tmp_x,&intime,&inspatial,tmp_clstr,k,
                           ic1,nc,&iter,&iseed,wss,&ier);

  if(type_clstr == NCL_float) {
    coerce_output_float_only(clstr,tmp_clstr,size_clstr,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_x     != NCL_double) NclFree(tmp_x);
  if(type_clstr != NCL_double) NclFree(tmp_clstr);

/*
 * Set up return value.
 */
  type_clstr_class = (NclTypeClass)(_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_clstr))));
  return_md = _NclCreateVal(
                            NULL,
                            NULL,
                            Ncl_MultiDValData,
                            0,
                            clstr,
                            NULL,
                            ndims_clstr,
                            dsizes_clstr,
                            TEMPORARY,
                            NULL,
                            (NclObjClass)type_clstr_class
                            );
/*
 * Initialize att_id so we can return some attributes.
 */
  att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

  ndims_atts     = 1;
  dsizes_atts[0] = ntime;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         ic1,
                         NULL,
                         ndims_atts,
                         dsizes_atts,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );

  _NclAddAtt(
             att_id,
             "id",
             att_md,
             NULL
             );

  ndims_atts     = 1;
  dsizes_atts[0] = *k;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         nc,
                         NULL,
                         ndims_atts,
                         dsizes_atts,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );

  _NclAddAtt(
             att_id,
             "npts",
             att_md,
             NULL
             );

  ndims_atts     = 1;
  dsizes_atts[0] = *k;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         wss,
                         NULL,
                         ndims_atts,
                         dsizes_atts,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypedoubleClass
                         );

  _NclAddAtt(
             att_id,
             "ss2",
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

  NclFree(dsizes_clstr);

  return(NhlNOERROR);
}

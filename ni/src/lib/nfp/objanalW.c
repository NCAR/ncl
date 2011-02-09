#include <stdio.h>
#include "wrapper.h"

extern void NGCALLF(dobjanlx,DOBJANLX)(double *, double *, double *, int *, 
                                       int *, double *, int *, int *, double *,
                                       double *, double *, int *, double *, 
                                       double *, double *, logical *, 
                                       double *, double *, double *, int *,
                                       double *, int *, int *);

#define NSCANMX 10

NhlErrorTypes obj_anal_ic_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *plon;
  double *tmp_plon;
  ng_size_t dsizes_plon[1];
  int has_missing_plon;
  NclScalar missing_plon, missing_flt_plon, missing_dbl_plon;
  NclBasicDataTypes type_plon;

/*
 * Argument # 1
 */
  void *plat;
  double *tmp_plat;
  ng_size_t dsizes_plat[1];
  int has_missing_plat;
  NclScalar missing_plat, missing_flt_plat, missing_dbl_plat;
  NclBasicDataTypes type_plat;

/*
 * Argument # 2
 */
  void *pval;
  double *tmp_pval;
  int ndims_pval;
  ng_size_t dsizes_pval[NCL_MAX_DIMENSIONS], size_pval;
  int has_missing_pval;
  NclScalar missing_pval, missing_flt_pval, missing_dbl_pval;
  NclBasicDataTypes type_pval;

/*
 * Argument # 3
 */
  void *glon;
  double *tmp_glon;
  ng_size_t dsizes_glon[1];
  NclBasicDataTypes type_glon;

/*
 * Argument # 4
 */
  void *glat;
  double *tmp_glat;
  ng_size_t dsizes_glat[1];
  NclBasicDataTypes type_glat;

/*
 * Argument # 5
 */
  void *rscan;
  double *tmp_rscan;
  ng_size_t dsizes_rscan[1];
  NclBasicDataTypes type_rscan;

/*
 * Argument # 6
 */
  logical *opt;
/*
 * Return variable
 */
  void *grid;
  double *tmp_grid;
  int ndims_grid;
  ng_size_t *dsizes_grid;
  NclScalar missing_grid, missing_dbl_grid;
  NclBasicDataTypes type_grid;


/*
 * Variables for retrieving attributes from the first argument.
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
  int set_wgts;
  double *wgts = NULL;
  double def_wgts[NSCANMX] = {1.0, 0.85, 0.70, 0.50, 0.25,0.25,0.25,0.25,
                              0.25,0.25};
  NclBasicDataTypes type_wgts = NCL_none;
/*
 * Various
 */
  double xmsg, *work, *zval, *zlat, *zlon;
  int ret, ier;
  int *ip, imlon, inlat, intim, inscan, ilwork, inpts;
  ng_size_t npts, ntim, mlon, nlat, nscan;
  ng_size_t i, size_output, lwork;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */
/*
 * Get argument # 0
 */
  plon = (void*)NclGetArgValue(
           0,
           7,
           NULL,
           dsizes_plon,
           &missing_plon,
           &has_missing_plon,
           &type_plon,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_plon,has_missing_plon,&missing_plon,
                 &missing_dbl_plon,&missing_flt_plon);

  npts = dsizes_plon[0];
/*
 * Get argument # 1
 */
  plat = (void*)NclGetArgValue(
           1,
           7,
           NULL,
           dsizes_plat,
           &missing_plat,
           &has_missing_plat,
           &type_plat,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_plat,has_missing_plat,&missing_plat,
                 &missing_dbl_plat,&missing_flt_plat);

  if(dsizes_plat[0] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: plat and plon must be the same length");
    return(NhlFATAL);
  }

/*
 * Get argument # 2
 */
  pval = (void*)NclGetArgValue(
           2,
           7,
           &ndims_pval,
           dsizes_pval,
           &missing_pval,
           &has_missing_pval,
           &type_pval,
           DONT_CARE);

/*
 * Coerce missing value to double if necessary.
 */
  coerce_missing(type_pval,has_missing_pval,&missing_pval,
                 &missing_dbl_pval,&missing_flt_pval);

  if(dsizes_pval[ndims_pval-1] != npts) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: The rightmost dimension of pval must be the same length as plat and plon");
    return(NhlFATAL);
  }

/*
 * Get argument # 3
 */
  glon = (void*)NclGetArgValue(
           3,
           7,
           NULL,
           dsizes_glon,
           NULL,
           NULL,
           &type_glon,
           DONT_CARE);

/*
 * Get argument # 4
 */
  glat = (void*)NclGetArgValue(
           4,
           7,
           NULL,
           dsizes_glat,
           NULL,
           NULL,
           &type_glat,
           DONT_CARE);


/*
 * Get argument # 5
 */
  rscan = (void*)NclGetArgValue(
           5,
           7,
           NULL,
           dsizes_rscan,
           NULL,
           NULL,
           &type_rscan,
           DONT_CARE);

  nscan = dsizes_rscan[0];
  if(nscan > NSCANMX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: 'nscan' must be <= 10");
    return(NhlFATAL);
  }

/*
 * Get argument # 6
 */
  opt = (logical*)NclGetArgValue(
           6,
           7,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

/*
 * Calculate size of leftmost dimensions (ntim).
 */
  ntim = 1;
  for(i = 0; i < ndims_pval-1; i++) ntim *= dsizes_pval[i];

/* 
 * Convert plat/plon to double if necessary.
 */
  tmp_plon = coerce_input_double(plon,type_plon,npts,has_missing_plon,
                                 &missing_plon,&missing_dbl_plon);
  tmp_plat = coerce_input_double(plat,type_plat,npts,has_missing_plat,
                                 &missing_plat,&missing_dbl_plat);
  if(tmp_plon == NULL || tmp_plat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for coercing plon/plat arrays to double");
    return(NhlFATAL);
  }
  if(has_missing_plon) {
    xmsg = missing_plon.doubleval;
  }
  else if(has_missing_plat) {
    xmsg = missing_plat.doubleval;
  }
  else {
    xmsg = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
  }

 /*
  * Coerce pval to double if necessary.
  */
  mlon        = dsizes_glon[0];
  nlat        = dsizes_glat[0];
  size_output = ntim * nlat * mlon;
  size_pval   = ntim * npts;

  tmp_pval = coerce_input_double(pval,type_pval,size_pval,has_missing_pval,
                                 &missing_pval,&missing_dbl_pval);
  if(tmp_pval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to coerce pval to double");
    return(NhlFATAL);
  }

/*
 * The output type defaults to float, unless the input array is double.
 */
  if(type_pval != NCL_double) {
    type_grid = NCL_float;
  }
  else {
    type_grid = NCL_double;
  }

/*
 * Convert glat/glon to double if necessary.
 */
  tmp_glon = coerce_input_double(glon,type_glon,mlon,0,NULL,NULL);
  tmp_glat = coerce_input_double(glat,type_glat,nlat,0,NULL,NULL);
  if(tmp_glon == NULL || tmp_glat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for coercing glat/glon to double");
    return(NhlFATAL);
  }

/*
 * Convert rscan to double if necessary.
 */
  tmp_rscan = coerce_input_double(rscan,type_rscan,nscan,0,NULL,NULL);
  if(tmp_rscan == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for coercing rscan to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for work arrays
 */
  lwork = 2*npts;
  work = (double *)calloc(lwork,sizeof(double));
  zval = (double *)calloc(npts*ntim,sizeof(double));
  zlat = (double *)calloc(npts,sizeof(double));
  zlon = (double *)calloc(npts,sizeof(double));
  ip   = (int *)calloc(npts,sizeof(int));
  if(work==NULL || zval==NULL || zlat==NULL || zlon==NULL || ip==NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  if(type_grid != NCL_double) {
    tmp_grid = (double *)calloc(size_output,sizeof(double));
    grid     = (void *)calloc(size_output, sizeof(float));
    if(grid == NULL || tmp_grid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
    grid = (void *)calloc(size_output, sizeof(double));
    if(grid == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    tmp_grid = grid;
  }
  if(has_missing_pval) {
    if(type_grid == NCL_double) missing_grid = missing_dbl_pval;
    else                        missing_grid = missing_flt_pval;
    missing_dbl_grid = missing_dbl_pval;
  }
  else {
    if(type_grid == NCL_double) {
      missing_dbl_grid.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      missing_grid.doubleval = missing_dbl_grid.doubleval;
    }
    else {
      missing_dbl_grid.doubleval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
      missing_grid.floatval = (float)missing_dbl_grid.doubleval;
    }
  }

/* 
 * Allocate space for output dimension sizes and set them.
 */
  ndims_grid = ndims_pval + 1;
  dsizes_grid = (ng_size_t*)calloc(ndims_grid,sizeof(ng_size_t));  
  if( dsizes_grid == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }
  for(i = 0; i < ndims_grid-2; i++) dsizes_grid[i] = dsizes_pval[i];
  dsizes_grid[ndims_grid-2] = nlat;
  dsizes_grid[ndims_grid-1] = mlon;

  set_wgts = 0;
  if(*opt) {
/* 
 * Check for a "blend_wgt" optional attribute attached to "opt".
 */
    stack_entry = _NclGetArg(6, 7, DONT_CARE);
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
 * att_id == -1 ==> no attributes specified
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
          if ((strcmp(attr_list->attname, "blend_wgt")) == 0) {
            if(attr_list->attvalue->multidval.dim_sizes[0] != nscan) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"obj_anal_ic: The 'blend_wgt' attribute must have 'nscan' weights");
              NhlPError(NhlWARNING,NhlEUNKNOWN,"             Will use default weights.");
            }
            else {
              type_wgts = attr_list->attvalue->multidval.data_type;
              wgts = coerce_input_double(attr_list->attvalue->multidval.val,
                                         type_wgts,nscan,0,NULL,NULL);
              set_wgts = 1;
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
  if((mlon > INT_MAX) || (nlat > INT_MAX) ||
     (ntim > INT_MAX) || (nscan > INT_MAX) ||
     (lwork > INT_MAX) || (npts > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: one or more input dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imlon = (int) mlon;
  inlat = (int) nlat;
  intim = (int) ntim;
  inscan = (int) nscan;
  ilwork = (int) lwork;
  inpts = (int) npts;


/*
 * If "blend_wgt" attribute not set by user, then set some defaults here.
 */
  if(!set_wgts) {
    wgts = (double*)malloc(nscan*sizeof(double));
    if(wgts == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"obj_anal_ic: Unable to allocate memory for weights");
      return(NhlFATAL);
    }
    for(i=0; i < nscan; i++) wgts[i] = def_wgts[i];
  }
  
/*
 * There are no leftmost dimensions to loop across, b/c the 
 * the Fortran routine handles this through its "ntim" variable.
 */
  NGCALLF(dobjanlx,DOBJANLX)(tmp_plon, tmp_plat, tmp_pval, &intim, &inpts,
                             tmp_grid, &imlon, &inlat, 
                             &xmsg, &missing_dbl_pval.doubleval, tmp_rscan,
                             &inscan, tmp_glat, tmp_glon, wgts, opt, zval, zlat,
                             zlon,ip,work,&ilwork,&ier);
/*
 * Coerce output back to float if necessary.
 */
  if(type_grid == NCL_float) {
    coerce_output_float_only(grid,tmp_grid,size_output,0);
  }

/*
 * Free unneeded memory.
 */
  if(type_plon  != NCL_double) NclFree(tmp_plon);
  if(type_plat  != NCL_double) NclFree(tmp_plat);
  if(type_pval  != NCL_double) NclFree(tmp_pval);
  if(type_glon  != NCL_double) NclFree(tmp_glon);
  if(type_glat  != NCL_double) NclFree(tmp_glat);
  if(type_rscan != NCL_double) NclFree(tmp_rscan);
  if(type_grid  != NCL_double) NclFree(tmp_grid);
  if(!set_wgts || (set_wgts && type_wgts != NCL_double)) NclFree(wgts);
  NclFree(work);
  NclFree(ip);
  NclFree(zval);
  NclFree(zlat);
  NclFree(zlon);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(grid,ndims_grid,dsizes_grid,&missing_grid,
                       type_grid,0);
  NclFree(dsizes_grid);
  return(ret);
}


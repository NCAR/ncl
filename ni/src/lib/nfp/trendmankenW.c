#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "wrapper.h"

extern void NGCALLF(kenstst,KENSTST)(double *, int *, int *, double *, 
                                     double *, int *, double *, 
                                     logical *, double *,int *);

extern void printmnmx(double*,int,const char*);

NhlErrorTypes trend_manken_W( void )
{

/*
 * Input variables
 */
/*
 * Argument # 0
 */
  void *x;
  double *tmp_x;
  int ndims_x;
  ng_size_t nx, dsizes_x[NCL_MAX_DIMENSIONS];
  int inx;
  NclBasicDataTypes type_x;

/*
 * Argument # 1
 */
  int *dims;
  ng_size_t ndims;
/*
 * Argument # 2
 */
  logical *opt;
/*
 * Return variable
 */
  void *tm;
  int ndims_tm;
  ng_size_t *dsizes_tm;
  NclBasicDataTypes type_tm;
/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry stack_entry;
  logical return_trend;

/*
 * Various
 */
  int s, ncomp;
  logical *tieflag;
  double z, prob, trend, eps, *slope;
  ng_size_t nslp, i, j, nrnx, total_nl, total_nr, total_elements, size_output;
  ng_size_t index_nrx, index_nr, index_x, index_tm;
  int inslp, ret;

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

/*
 * Get argument # 1
 */
  opt = (logical*)NclGetArgValue(
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
  dims = (int *)NclGetArgValue(2,3,NULL,&ndims,NULL,NULL,NULL,DONT_CARE);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
  for(i = 0; i < ndims; i++ ) {
    if(dims[i] < 0 || dims[i] >= ndims_x) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Invalid dimension sizes to do calculation on, can't continue");
      return(NhlFATAL);
    }
    if(i > 0 && dims[i] != (dims[i-1]+1)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Input dimension sizes must be monotonically increasing, can't continue");
      return(NhlFATAL);
    }
  }

/*
 * Check for "return_trend" attribute attached to "opt".
 *
 * Before NCL Version 6.4.0, trend_manken always returned both
 * probability and trend, regardless of what "opt" was set to. Some folks
 * don't want the trend, so in NCL V6.4.0 an attribute was added,
 * "return_trend" that allows you to turn off the trend calculation.
 *
 * Both opt = True AND opt@return_trend = False must be set in order to
 * NOT calculate trend. This was to maintain backwards compatibility.
 */
  return_trend = True;
  if(*opt) {
    stack_entry = _NclGetArg(1, 3, DONT_CARE);
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
          if(!strcasecmp(attr_list->attname, "return_trend")) {
            return_trend = *(logical *) attr_list->attvalue->multidval.val;
          }
          attr_list = attr_list->next;
        }
      default:
        break;
      }
    }
  }


/* 
 * Allocate space for output dimension sizes and set them.  We either
 * calculate both the prob and the trend (opt=False), or just prob
 * (opt=True).  If both, then the return array will either be 2 x M, or
 * just 2 (if the input array is 1D).  Otherwise, hen the return array is
 * just M or a scalar ((if input array is 1D).
 */
  if(return_trend) {
    ndims_tm = ndims_x - ndims + 1;
  }
  else {
    ndims_tm = max(ndims_x - ndims,1);
  }

  dsizes_tm = (ng_size_t*)calloc(ndims_tm,sizeof(ng_size_t));  
  if( dsizes_tm == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for holding dimension sizes");
    return(NhlFATAL);
  }

  if(return_trend) {
    if(ndims_tm == 1) j = 0;
    else              j = 1;
    dsizes_tm[0] = 2;
  }
  else {
    j = 0;
    dsizes_tm[0] = 1;  /* just in case there's just one value. */
  }
  nx = total_nl = total_nr = 1;
  for(i = 0; i < dims[0]; i++) {
    total_nl *= dsizes_x[i];
    dsizes_tm[j+i] = dsizes_x[i];
  }
  for(i = 0; i < ndims ; i++) {
    nx = nx*dsizes_x[dims[i]];
  }
  for(i = dims[ndims-1]+1; i < ndims_x; i++) {
    total_nr *= dsizes_x[i];
    dsizes_tm[j+i-ndims] = dsizes_x[i];
  }

  if(nx > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: nx = %ld is greater than INT_MAX", nx);
    return(NhlFATAL);
  }

  nslp = nx*(nx-1)/2;
  if(nslp > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: nslp = %ld is greater than INT_MAX", nslp);
    return(NhlFATAL);
  }
  inslp = (int) nslp;
  inx   = (int) nx;

/*
 * Allocate space for tmp_x.
 */
  tmp_x = (double *)calloc(nx,sizeof(double));
  if(tmp_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for coercing input array to double");
    return(NhlFATAL);
  }

/* 
 * Allocate space for output array.
 */
  total_elements = total_nr * total_nl;
  if(return_trend) size_output    = 2 * total_elements;
  else             size_output    = total_elements;
  if(type_x != NCL_double) {
    type_tm = NCL_float;
    tm = (void *)calloc(size_output, sizeof(float));
  }
  else {
    type_tm = NCL_double;
    tm = (void *)calloc(size_output, sizeof(double));
  }
  if(tm == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/* 
 * Allocate space for various other arrays.
 */
  slope = (double *)calloc(nslp, sizeof(double));
  if(slope == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for slope array");
    return(NhlFATAL);
  }
  tieflag = (logical *)calloc(nx, sizeof(logical));
  if(tieflag == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"trend_manken: Unable to allocate memory for internal logical array");
    return(NhlFATAL);
  }
  eps = 1.0e-5;

/*
 * Loop across rightmost/leftmost dimensions and call 
 * the Fortran routine for each subsection of the 
 * input arrays.
 */
  nrnx = total_nr * nx;
  for(i = 0; i < total_nl; i++) {
    index_nrx = i*nrnx;
    index_nr  = i*total_nr;
    for(j = 0; j < total_nr; j++) {
      index_x  = index_nrx + j;
      index_tm = index_nr + j;
/*
 * Coerce subsection of x (tmp_x) to double.
 */
      coerce_subset_input_double_step(x,tmp_x,index_x,total_nr,type_x,
                                      nx,0,NULL,NULL);
/*
 * Call the Fortran routine.
 */
      NGCALLF(kenstst,KENSTST)(tmp_x, &inx, &s, &z, &prob, &inslp,
                               slope, tieflag, &eps, &ncomp);
/*
 * Coerce prob as necessary.
 */
      coerce_output_float_or_double(tm,&prob,type_tm,1,index_tm);

 /*
  * To calculate trend, we must sort first. Note: ncomp is 
  * returned in range [1 to n], not [0,n-1],so we have to 
  * subtract 1.
  */
      if(return_trend) {
        qsort((void*)slope,ncomp,sizeof(double),cmpdouble);
        if((ncomp % 2) == 0) {
          trend = 0.5*(slope[(ncomp-1)/2]+slope[(ncomp-1)/2+1]);
        }
        else {
          trend = slope[(ncomp-1)/2+1];
        }
/*
 * Coerce trend as necessary.
 */
        coerce_output_float_or_double(tm,&trend,type_tm,1,index_tm+total_elements);
      }
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(tmp_x);
  NclFree(slope);
  NclFree(tieflag);

/*
 * Return value back to NCL script.
 */
  ret = NclReturnValue(tm,ndims_tm,dsizes_tm,NULL,type_tm,0);

  NclFree(dsizes_tm);
  return(ret);
}

void printmnmx(double *data,int nx, const char *name)
{
  double dmin, dmax;
  int i, idmin,idmax;
  dmin = dmax = data[0];
  idmin = idmax = 1;
  for(i = 1; i < nx; i++) {
    if(data[i] > dmax) {
      idmax = i;
      dmax = data[i];
    }
    if(data[i] < dmin) {
      idmin = i;
      dmin = data[i];
    }
  }
  printf("=======================================================\n");
  printf("name = %s\n",name);
  printf("min,max %g %g\n", dmin,dmax);
  printf("index %d %d\n",idmin,idmax);
}


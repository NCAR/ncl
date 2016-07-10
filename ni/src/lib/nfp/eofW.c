#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(ddrveof,DDRVEOF)(double *,int *,int *,int *,int *,
                                     double *,int *,double *, double *,
                                     float*,double *,int *,int *,double*,
                                     long long int *, double *,int *,
                                     double *,int *,int *,int *,int *,int *);

extern void NGCALLF(xrveoft,XRVEOFT)(double *dx_strip, double *dx_strip_t,
                                     int *nrow, int *ncol, int *nrobs,
                                     int *mcsta, double *xmsg, int *neval,
                                     double *eval, double *evec,
                                     double *pcvar, double *trace,
                                     double *xdvar, double *xave, 
                                     int *jopt, int *ier);


extern void NGCALLF(deof11,DEOF11)(double *, int *, int *, int *, int *,
                                   double  *, double *, double *, double *,
                                   double *);

extern void NGCALLF(dncldrv,DNCLDRV)(double *,double *,int *,int *,int *,
                                     int *,double *,int *,double *,double *,
                                     float *,double *,int *,int *,double *,
                                     double *,double *,long long int *,
                                     double *, int *,double *,int *,int *,
                                     int *,int *,int*);

extern void NGCALLF(deofts7,DEOFTS7)(double *,int *,int *,int *,int *,
                                     double *,int *, double *,int *, int *,
                                     double *,double *,double *,double *,
                                     int *);

extern void NGCALLF(deoftsca,DEOFTSCA)(double *,int *,int *,int *,int *,
                                       double *,int *,double *,int *, int*,
                                       double *,double *,int *,double *,
                                       double *);

extern void NGCALLF(dtncleof,DTNCLEOF)(double *, int *, int *, int *, int *,
                                       double *, int *, double *, double *,
                                       float *, double *, int *, int *,
                                       double *,  double *, double *, 
                                       double *, double *, double *,
                                       long long int *, double *, int *, 
                                       int *, int *, int *, int *, int *);

extern void NGCALLF(deof2data,DEOF2DATA)(int *,int *,int *,double *,
                                         double *, double *, double *);

extern void NGCALLF(dstat2,DSTAT2)(double *, int *, double *, double *,
                                   double *, double *, int *, int *);

/*
 * This routine calls three different EOF routines.
 *
 * The first is the original eofcov routine, which can be
 * extremely slow if  nrow < mcsta.
 *
 * The second routine is one Dennis wrote in 2004/2005 to speed
 * up the case where nrow < mcsta. This routine had a problem with
 * one particular case with a French grid. Dennis spent quite a
 * bit of time proving that this routine works with several
 * textbook examples, but he's not certain why it is having problems
 * with this one particular grid.
 *
 * The third routine was taken from SCRIPPS and modified by Dennis
 * to handle missing values.
 *
 * If use_old_transpose = use_new_transpose = False, then the old
 * routine is used.  If use_old_transpose = True, then Dennis' transpose
 * routine is used. If use_new_transpose = True, then SCRIPPS transpose
 * routine is used. Note: use_old_tranpose should only be used for
 * debugging purposes. It is not intended to be used by the user.
 *
 */


NhlErrorTypes eofunc_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  logical *opt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  ng_size_t nrow, ncol, nobs, msta, mcsta, nc, nc2, nr;
  int inrow, incol, inobs, imcsta, kntx;
  ng_size_t total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  float  scale_factor;
  double *pcrit = NULL;
  float *rpcrit = NULL;
  NclBasicDataTypes type_pcrit = NCL_none;
  ng_size_t i, j, l1, l2;
  int iopt = 0, jopt = 0, ier = 0;
  logical tr_setbyuser = False, anomalies = False, debug = False;
  logical use_new_transpose = False, use_old_transpose = False;
  logical return_eval = True, return_trace = False, return_pcrit = False;
/*
 * Work array variables.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *cssm = NULL, *work = NULL, *weval = NULL;
  int   *iwork = NULL, *ifail = NULL;
  ng_size_t lwork, liwork, lifail, lweval;
  int icovcor, ilwork, iliwork, ilifail;
  long long int llcssm;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix, *cmethod;
  NclQuark *matrix, *method;
  double *trace = NULL, *eval, *eval2, *pcvar = NULL, *prncmp = NULL;
  float *rpcvar = NULL, *rtrace, *reval, *reval2;
/*
 * Output array variables
 */
  double *evec = NULL, *wevec, *xdatat = NULL;
  float *revec = NULL;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * Get option.
 */
  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || 
     (msta > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow = (int) nrow;
  incol = (int) ncol;
  inobs = (int) nobs;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
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
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "jopt"        : both routines
 *   "return_eval" : both routines (unadvertised)
 *   "return_trace": return trace
 *   "return_pcrit": return pcrit
 *   "pcrit"       : transpose routine only
 *   "anomalies"   : If True, anomalies have already been calculated by
 *                   user, and this interface shouldn't remove them.
 *   "transpose"   : If True, call transpose routine no matter what
 *                 : If False, don't call transpose routine no matter what
 *   "oldtranspose": If True, call Dennis' old transpose routine.
 *   "debug"       : turn on debug
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
            }
          }
/*
 * Check for "pcrit". If user sets this attribute, then we'll return
 * it as an attribute of the return variable.
 */
          if(!strcmp(attr_list->attname, "pcrit")) {
            type_pcrit   = attr_list->attvalue->multidval.data_type;
            return_pcrit = True;
/*
 * If "pcrit" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_pcrit == NCL_double) {
              pcrit  = (double *)calloc(1,sizeof(double));
              *pcrit = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_pcrit == NCL_int || type_pcrit == NCL_float) {
/*
 * Coerce to double.
 */
              pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_pcrit,1,0,NULL,NULL);
/*
 * For later, when we return "pcrit" as an attribute of the return value.
 */
              type_pcrit = NCL_float;
              rpcrit  = (float *)calloc(1,sizeof(float));
              *rpcrit = (float)(*pcrit);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'pcrit' attribute must be of type numeric. Defaulting to 50.");
              return_pcrit = False;
            }
          }
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "return_eval")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_eval = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'return_eval' attribute must be a logical. Defaulting to False.");
            }
          }
/*
 * Check for "return_trace".
 */
          if (!strcmp(attr_list->attname, "return_trace")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_trace = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'return_trace' attribute must be a logical. Defaulting to False.");
            }
          }
/*
 * Check for "anomalies".
 */
          if (!strcmp(attr_list->attname, "anomalies")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              anomalies = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'anomalies' attribute must be a logical. Will default to False");
            }
          }
/*
 * Check for "transpose".
 */
          if (!strcmp(attr_list->attname, "transpose")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              use_new_transpose = *(logical*) attr_list->attvalue->multidval.val;
              tr_setbyuser = True;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'transpose' attribute must be a logical. Will let routine pick best value.");
            }
          }
/*
 * Check for "oldtranspose".
 */
          if (!strcmp(attr_list->attname, "oldtranspose")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              use_old_transpose = *(logical*) attr_list->attvalue->multidval.val;
              tr_setbyuser = True;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'oldtranspose' attribute must be a logical. Will default to False.");
            }
          }
/*
 * Check for "debug".
 */
          if (!strcmp(attr_list->attname, "debug")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              debug = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The 'debug' attribute must be a logical. Defaulting to False.");
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
 * If user didn't set pcrit, then set it here.
 */
  if(!return_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    if( pcrit == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for pcrit");
      return(NhlFATAL);
    }
    *pcrit = 50.;
  }
  if(debug) {
    printf("eofunc: pcrit = %g\n", *pcrit);
    if(!anomalies) {
      printf("anomalies being removed...\n");
    }
    else {
      printf("anomalies NOT being removed...\n");
    }
  }

/*
 * Create arrays to store non-missing data and to remove mean from
 * data before entering Fortran routines.
 */
  dx_strip = (double *)calloc(nrow * ncol,sizeof(double));
  xave     = (double *)calloc(ncol,sizeof(double));
  xvar     = (double *)calloc(ncol,sizeof(double));
  xdvar    = (double *)calloc(ncol,sizeof(double));
  if( dx_strip == NULL || xave == NULL || xvar == NULL || xdvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

/*
 * Strip all grid points that have less than "PCRIT" valid values.
 * Create "dx_strip". This may have fewer columns/grid-pts
 * than the original "dx" array, if not all columns 
 * had the minimum number of valid values.
 */
  mcsta = 0;

  for( nc = 0; nc < ncol; nc++) {
/*
 * Statistics for this station/grid-point
 */
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&inrow,&missing_dx.doubleval,
                           &xave[nc],&xvar[nc],&xsd,&kntx,&ier);
/*
 * Eliminate stations/grid-points with less than pcrit % of data.
 */
    pcx = ((double)kntx/(double)nrow)*100.;
    if (pcx < *pcrit || xsd <= 0.0) {
      xave[nc] = missing_dx.doubleval;
    }
/* 
 * Create anomalies. If jopt=1, then normalize the anomalies.
 * mcsta is the number of acceptable grid/station points (mcsta <= msta).
 */
    con = 1.0;
    if(jopt == 1 && xave[nc] != missing_dx.doubleval && xsd > 0.0) {
      con = 1./xsd;
    }     
/*
 * Work with anomalies: xdave=0.0 [or standardized anomalies]
 */
    if (xave[nc] != missing_dx.doubleval) {
/*
 * The following can produce too much output, so I've commented it out.
 *
 *      if(debug) {
 *          printf("nc = %d xave = %g\n", nc, xave[nc]);
 *      }
 */
/*
 * Increment counter for acceptable points.
 */
      for( nr = 0; nr < nobs; nr++) {
        if(dx[nc*nrow+nr] != missing_dx.doubleval) {
          if(!anomalies) {
/*
 * User hasn't removed anomalies, so do it here.
 */
            dx_strip[mcsta*nrow+nr] = (dx[nc*nrow+nr] - xave[nc]) * con;
          }
          else {
            if(debug) {
              printf("anomalies NOT being removed...\n");
            }
/*
 * User has already removed anomalies, so leave alone.
 */
            dx_strip[mcsta*nrow+nr] = dx[nc*nrow+nr];
          }
        }
        else {
          dx_strip[mcsta*nrow+nr] = missing_dx.doubleval;
        }
      }
      if(jopt == 0) {
        xdvar[mcsta] = xvar[nc];
      }
      else {
        xdvar[mcsta] = 1.0;
      }
      mcsta++;
    }
  }

  if(mcsta > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imcsta = (int) mcsta;

/*
 * Depending on the size of the rightmost 2D arrays being processed, and/or
 * the value of the transpose or oldtranspose attributes, we call one of
 * three different Fortran routines. These routines basically behave the
 * same, except two of them operate on a transposed version of the 2d array.
 */
  if(debug) {
    printf("eofunc: msta = %ld mcsta = %ld nobs = %ld\n", msta, mcsta, nobs);
  }
/*
 * If one of the transpose attributes has not explicitly been set by the
 * user, then based on the sizes of the input array's dimensions, determine 
 * whether to call a transpose routine or not.
 */
  if(!tr_setbyuser) {
/*
 * If mcsta <= nrow, don't call transpose routine.
 */
    if(mcsta <= nrow) {
      use_new_transpose = False;    /* already the default */
      use_old_transpose = False;
      if(debug) {
        printf("eofunc: transpose set to False\n");
      }
    }
    else {
/*
 * Since mcsta > nrow, call transpose routine. 
 */
      use_new_transpose = True;
      use_old_transpose = False;
      if(debug) {
          printf("eofunc: transpose set to True\n");
      }
    }
  }
  else if(debug) {
/*
 * User explicitly set one of the transpose attributes, so indicate
 * which one here. Note that if both oldtranspose and transpose are
 * set to True, transpose will take precedence.
 */
    if(use_new_transpose) {
      printf("eofunc: user set use_new_transpose to True\n");
    }
    else if(use_old_transpose) { 
      printf("eofunc: user set use_old_transpose to True\n");
    }
    else {
      printf("eofunc: user set neither transpose attribute to True\n");
    }
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];
  total_size_evec = *neval * ncol;

/*
 * Allocate memory for various arrays.  Depending on which Fortran routine
 * will be called later, different quantities need to be allocated here.
 */
  if(use_new_transpose) {
    xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
    wevec  = (double *)calloc(*neval * mcsta,sizeof(double));
    prncmp = (double *)calloc(*neval*nrow,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    pcvar  = (double *)calloc(*neval,sizeof(double));
    if(xdatat == NULL || wevec == NULL || prncmp == NULL || 
       eval == NULL || pcvar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
/*
 * Determine whether the return eigenvectors will be float or double,
 * and allocate space if necessary.
 */
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
/*
 * If mcsta = msta, then we can use wevec as is. Otherwise, later we
 * need to copy wevec to locations in which the input was not missing.
 */
      if(mcsta != msta) {
        evec = (double*)calloc(total_size_evec,sizeof(double));
        if( evec == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for output array");
          return(NhlFATAL);
        }
      }
    }
/*
 * Transpose the input array.
 */
    l1=0;
    for(i = 0; i < mcsta; i++ ) {
      l2 = i;  
      for(j = 0; j < nrow; j++ ) {
        xdatat[l2] = dx_strip[l1];
        l1++;
        l2+=mcsta;

      }
    }
/*
 * Initialization for other arrays.
 */
    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      pcvar[ne] = eval[ne] = missing_dx.doubleval;
      for( nc = 0; nc < mcsta; nc++) {
        wevec[i] = missing_dx.doubleval;
        i++;
      }
    }
  }
  else if(use_old_transpose) {
    trace  = (double *)calloc(1,sizeof(double));
    evec   = (double*)calloc(total_size_evec,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    pcvar  = (double *)calloc(*neval,sizeof(double));
    xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
    if(trace == NULL || pcvar == NULL || eval == NULL || xdatat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
/*
 * Initialization.
 */
    *trace = missing_dx.doubleval;
    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      pcvar[ne] = eval[ne] = missing_dx.doubleval;
      for( nc = 0; nc < ncol; nc++) {
        evec[i] = missing_dx.doubleval;
        i++;
      }
    }
  }
  else {
/*
 * eofcov routine
 *
 * Allocate space needed for various arrays.
 */
    wevec  = (double *)calloc(total_size_evec,sizeof(double));
    trace  = (double *)calloc(1,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    rpcvar = (float *)calloc(*neval,sizeof(float));
    if(wevec == NULL || trace == NULL || rpcvar == NULL || eval == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
/*
 * If mcsta = msta, then we can use wevec as is. Otherwise, later we
 * need to copy wevec to locations in which the input was not missing.
 */
      if(mcsta != msta) {
        evec = (double*)calloc(total_size_evec,sizeof(double));
        if( evec == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for output array");
          return(NhlFATAL);
        }
      }
    }

/*
 * Check sizes of work arrays that need to be passed to Fortran 
 * routine below.
 */
    llcssm = mcsta*(mcsta+1)/2;
    lwork  = 8*mcsta;
    liwork = 5*mcsta;
    lifail = mcsta;

    if((lwork > INT_MAX) || (liwork > INT_MAX) || (lifail > INT_MAX)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: one or more dimension sizes is greater than INT_MAX");
      return(NhlFATAL);
    }
    ilwork  = (int) lwork; 
    iliwork = (int) liwork; 
    ilifail = (int) lifail; 

/*
 * Initialization.
 */
    *trace = missing_dx.doubleval;

    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      eval[ne]   = missing_dx.doubleval;
      rpcvar[ne] = (float)missing_dx.doubleval;
      for( nc = 0; nc < ncol; nc++) {
        wevec[i] = missing_dx.doubleval;
        i++;
      }
    }
/*
 * Create some work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
    lweval = lifail;
    cssm   = (double *)calloc(llcssm,sizeof(double));
    work   = (double *)calloc(lwork,sizeof(double));
    weval  = (double *)calloc(lweval,sizeof(double));
    iwork  =    (int *)calloc(liwork,sizeof(int));
    ifail  =    (int *)calloc(lifail,sizeof(int));
    if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL ||
        ifail == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for work arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran 77 version of appropriate routine.
 */
  if(use_new_transpose) {
    icovcor = 0;
    NGCALLF(deof11,DEOF11)(xdatat,&imcsta,&inrow,neval,&icovcor,
                           &missing_dx.doubleval,eval,wevec,pcvar,prncmp);
  }
  else if(use_old_transpose) {
    NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&inrow,&incol,&inobs,&imcsta,
                             &missing_dx.doubleval,neval,eval,evec,
                             pcvar,trace,xdvar,xave,&jopt,&ier);
  }
  else {
    NGCALLF(ddrveof,DDRVEOF)(dx_strip,&inrow,&incol,&inobs,&imcsta,
                             &missing_dx.doubleval,neval,eval,wevec,rpcvar,
                             trace,&iopt,&jopt,cssm,&llcssm,work,&ilwork,
                             weval,iwork,&iliwork,ifail,&ilifail,&ier);
  }
/*
 * If we used the "old" transpose routine, then the returned eigenvectors
 * have already been returned to the original-sized array with all the
 * missing values in the correct locations.  All we need to do here is 
 * convert to float if necessary.
 */
  if(use_old_transpose) {
    if(type_x != NCL_double) {
      for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];
/*
 * Free up double precision array.
 */
      NclFree(evec);
    }
  }
/*
 * If we are dealing with the old eofcov routine, or the new SCRIPPS
 * routine, then we need to reshape the evec (or revec if float)
 * array.  Note  that for the old eofcov routine, wevec is actually
 * the same size as evec, whereas for the new routine, it's the same 
 * size only if mcsta == msta. 
 */
  else {
    if(mcsta < msta) {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 *
 * This is the floating point (single precision) case.
 */
      if(type_x != NCL_double) {
        for(i = 0; i < total_size_evec; i++) {
          revec[i] = (float)missing_dx.doubleval;
        }
/*
 * Now copy over the appropriate values in the wevec array. Since the
 * wevec array is a different size depending on which routine you are
 * using, we have two different sections of code here.
 */
        if(use_new_transpose) {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                revec[ne*ncol+nc] = (float)wevec[ne*mcsta+nc2];
              }
              nc2++;
            }
          }
        }
        else {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                revec[ne*ncol+nc] = (float)wevec[ne*ncol+nc2];
              }
              nc2++;
            }
          }
        }
      }
/*
 * This is the double precision case.
 */
      else {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 */
        for(i = 0; i < total_size_evec; i++) {
          evec[i] = missing_dx.doubleval;
        }
/*
 * Now copy over the appropriate values in the wevec array. Since the
 * wevec array is a different size depending on which routine you are
 * using, we have two different sections of code here.
 */
        if(use_new_transpose) { 
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                evec[ne*ncol+nc] = wevec[ne*mcsta+nc2];
              }
              nc2++;
            }
          }
        }
        else {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                evec[ne*ncol+nc] = wevec[ne*ncol+nc2];
              }
              nc2++;
            }
          }
        }
      }
      NclFree(wevec);
    }
    else {
/*
 * mcsta = msta, so we just need to copy stuff over. It doesn't matter
 * whether we have called the old eofcov routine or the new eof SCRIPPS
 * routine, because if mcsta==msta, then wevec is the same size for
 * both routines.
 */
      if(type_x != NCL_double) {
        for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)wevec[i];
        NclFree(wevec);
      }
      else {
        evec = wevec;
      }
    }
  }

/*
 * Check various possible error messages. The new transpose routine doesn't
 * have an ier.
 */
  if (!use_new_transpose && ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc: %d eigenvectors failed to converge",ier);
    }
  }

/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(dx_strip);
  NclFree(xave);
  NclFree(xvar);
  NclFree(xdvar);
  if(!use_new_transpose && !use_old_transpose) {
    NclFree(work);
    NclFree(cssm);
    NclFree(weval);
    NclFree(iwork);
    NclFree(ifail);
  }
  else {
    NclFree(xdatat);
    if(use_new_transpose) NclFree(prncmp);
  }

/*
 * This is the start of a rather large if-else statement. It is based
 * on whether you are returning floats or doubles. 
 */
  if(type_x != NCL_double) {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * Coerce eval to float.
 */
      reval = (float *)calloc(*neval,sizeof(float));
      if( reval == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for eigenvalue array");
        return(NhlFATAL);
      }
      for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * If we didn't use the SCRIPPS routine, then the eigenvalues
 * returned are okay as is. Otherwise, we have to apply a scale
 * factor and return both the original values and the scaled values.
 */
      if(use_new_transpose) {
        reval2 = (float *)calloc(*neval,sizeof(float));
        if( reval2 == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for eigenvalue array");
          return(NhlFATAL);
        }
        scale_factor = (mcsta-1)/(nrow-1);
        for( i = 0; i < *neval; i++ ) reval2[i] = scale_factor * reval[i];
/*
 * First return original eigenvalues as "eval_transpose".
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval_transpose",
                 att_md,
                 NULL
                 );
/*
 * Now return scaled eigenvalues as simply "eval".
 */
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval2,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
      else {
/*
 * We didn't call the tranpose routine, so we only need to return
 * one set of eigenvalues. 
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
/*
 * Free up original eval array, since we don't need it anymore.
 */
      NclFree(eval);
    }
/*
 * Only return the trace if the appropriate option has been set.
 * The new transpose routine doesn't return trace.
 */
    if(!use_new_transpose) {
      if(return_trace) {
/*
 * Coerce trace to float.
 */
        rtrace = (float *)calloc(1,sizeof(float));
        *rtrace = (float)(*trace);
        dsizes[0] = 1;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rtrace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                   att_id,
                   "trace",
                   att_md,
                   NULL
                   );
      }
      NclFree(trace);
    }
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * If we didn't use the SCRIPPS routine, then the eigenvalues
 * returned are okay as is. Otherwise, we have to apply a scale
 * factor and return both the original values and the scaled values.
 */
      if(use_new_transpose) {
        eval2 = (double *)calloc(*neval,sizeof(double));
        if( eval2 == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: Unable to allocate memory for eigenvalue array");
          return(NhlFATAL);
        }
        scale_factor = (mcsta-1)/(nrow-1);
        for( i = 0; i < *neval; i++ ) eval2[i] = scale_factor * eval[i];
/*
 * First return original eigenvalues as "eval_transpose".
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval_transpose",
                 att_md,
                 NULL
                 );
/*
 * Now return scaled eigenvalues as simply "eval".
 */
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval2,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
      else {
/*
 * We didn't call the tranpose routine, so we only need to return
 * one set of eigenvalues. 
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                               NULL,
                               NULL,
                               Ncl_MultiDValData,
                               0,
                               (void*)eval,
                               NULL,
                               1,
                               dsizes,
                               TEMPORARY,
                               NULL,
                               (NclObjClass)nclTypedoubleClass
                               );
        _NclAddAtt(
                   att_id,
                   "eval",
                   att_md,
                   NULL
                   );
      }
    }

    if(!use_new_transpose) {
      if(return_trace) {
        dsizes[0] = 1;  
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)trace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                   att_id,
                   "trace",
                   att_md,
                   NULL
                   );
      }
      else {
        NclFree(trace);
      }
    }
  }

/*
 * Return pcvar as float no matter what.
 */
  if(use_old_transpose || use_new_transpose) {
    rpcvar = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) rpcvar[i] = (float)pcvar[i];
    NclFree(pcvar);
  }
  dsizes[0] = *neval;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rpcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar",
             att_md,
             NULL
             );
/*
 * Only return "pcrit" if it was set by the user and we called one
 * of the transpose routines. The type returned is a float or a double,
 * depending on what pcrit was set to in the input.
 */
  if((use_new_transpose || use_old_transpose) && return_pcrit) {
    dsizes[0] = 1;
    if(type_pcrit == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rpcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)pcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
    }
    _NclAddAtt(
               att_id,
               "pcrit",
               att_md,
               NULL
               );
  }
  
/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
             att_md,
             NULL
             );

/*
 * "method" indicates whether the transpose routine was called or not.
 */
  if(use_new_transpose) {
    cmethod = (char *)calloc(10,sizeof(char));
    strcpy(cmethod,"transpose");
  }
  else if(use_old_transpose) {
    cmethod = (char *)calloc(14,sizeof(char));
    strcpy(cmethod,"old_transpose");
  }
  else {
    cmethod = (char *)calloc(13,sizeof(char));
    strcpy(cmethod,"no transpose");
  }
  method  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *method = NrmStringToQuark(cmethod);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)method,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "method",
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
 * Free memory 
 */
  NclFree(cmatrix);
  NclFree(cmethod);
  if((return_pcrit && type_pcrit != NCL_double) || !return_pcrit)  {
    NclFree(pcrit); 
  }

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);

  return(NhlNOERROR);
}


NhlErrorTypes eofunc_n_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx, *dx_orig;
  logical *opt;
  int *dim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  ng_size_t nrow, ncol, nobs, msta, mcsta, nc, nc2, nr;
  int inrow, incol, inobs, imcsta, kntx;
  ng_size_t total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  float  scale_factor;
  double *pcrit = NULL;
  float *rpcrit = NULL;
  NclBasicDataTypes type_pcrit = NCL_none;
  ng_size_t i, j, l1, l2;
  int iopt = 0, jopt = 0, ier = 0;
  logical tr_setbyuser = False, anomalies = False, debug = False;
  logical use_new_transpose = False, use_old_transpose = False;
  logical return_eval = True, return_trace = False, return_pcrit = False;
  ng_size_t nl, nm, counter, ireordered;
  ng_size_t size_leftmost, size_middle, size_rightmost, size_middle_rightmost;
  ng_size_t left_loc, mid_loc;
/*
 * Work array variables.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *cssm = NULL, *work = NULL, *weval = NULL;
  int   *iwork = NULL, *ifail = NULL;
  ng_size_t lwork, liwork, lifail, lweval;
  int icovcor, ilwork, iliwork, ilifail;
  long long int llcssm;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix, *cmethod;
  NclQuark *matrix, *method;
  double *trace = NULL, *eval, *eval2, *pcvar = NULL, *prncmp = NULL;
  float *rpcvar = NULL, *rtrace, *reval, *reval2;
/*
 * Output array variables
 */
  double *evec = NULL, *wevec, *xdatat = NULL;
  float *revec = NULL;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           4,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            4, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * Get option.
 */
  opt = (logical *)NclGetArgValue(
            2,
            4, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

 /*
  * Retrieve the dimension index for the "time" dimension.
  */ 
  dim = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Invalid dimension index for 'time'");
    return(NhlFATAL);
  }

/*
 * Calculate the left, middle and right dimensions, so we
 * can reorder array. The other ncol, ncor, nobs variables
 * are a bit confusing, but they are used as various 
 * arguments to the Fortran eof routine.
 * 
 * The important thing here is that size_leftmost * size_middle * size_rightmost
 * should equal the total size of x (total_size_x). Also, size_middle
 * is the "time" dimension that is specified by the "dim" argument 
 * above.
 * 
 *
 */
  size_rightmost = size_leftmost = 1;
  for( i = 0; i < *dim; i++ ) size_leftmost *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) size_rightmost *= dsizes_x[i];
  ncol = msta = size_rightmost * size_leftmost;
  size_middle = nobs = nrow = dsizes_x[*dim];
  size_middle_rightmost = size_rightmost * size_middle;
  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {

    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || 
     (msta > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow = (int) nrow;
  incol = (int) ncol;
  inobs = (int) nobs;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double.
 * Note: coerce_subset_input_double will force the coercion no matter 
 * what, which we need for x because we may have to reorder it.
 */

  dx_orig = (double*)malloc(total_size_x*sizeof(double));
  dx      = (double*)malloc(total_size_x*sizeof(double));
  if( dx_orig == NULL || dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
  coerce_subset_input_double(x,dx_orig,0,type_x,total_size_x,0,NULL,NULL);

/*
 * Create a vector containing the reordered indices, and also
 * copy reordered data into new array.
 */
  counter = 0;
  for(nl = 0; nl < size_leftmost; nl++) {
    left_loc = nl * size_middle_rightmost;
    for(nr = 0; nr < size_rightmost; nr++) {
      for(nm = 0; nm < size_middle; nm++) {
        mid_loc = nm * size_rightmost;
        ireordered = left_loc + mid_loc + nr;
        ((double*)dx)[counter] = ((double*)dx_orig)[ireordered];
        counter++;
      }
    }
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
  if(*opt) {
    stack_entry = _NclGetArg(2, 4, DONT_CARE);
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
 *
 *   "jopt"        : both routines
 *   "return_eval" : both routines (unadvertised)
 *   "return_trace": return trace
 *   "return_pcrit": return pcrit
 *   "pcrit"       : transpose routine only
 *   "anomalies"   : If True, anomalies have already been calculated by
 *                   user, and this interface shouldn't remove them.
 *   "transpose"   : If True, call transpose routine no matter what
 *                 : If False, don't call transpose routine no matter what
 *   "oldtranspose": If True, call Dennis' old transpose routine.
 *   "debug"       : turn on debug
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
            }
          }
/*
 * Check for "pcrit". If user sets this attribute, then we'll return
 * it as an attribute of the return variable.
 */
          if(!strcmp(attr_list->attname, "pcrit")) {
            type_pcrit   = attr_list->attvalue->multidval.data_type;
            return_pcrit = True;
/*
 * If "pcrit" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_pcrit == NCL_double) {
              pcrit  = (double *)calloc(1,sizeof(double));
              *pcrit = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_pcrit == NCL_int || type_pcrit == NCL_float) {
/*
 * Coerce to double.
 */
              pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_pcrit,1,0,NULL,NULL);
/*
 * For later, when we return "pcrit" as an attribute of the return value.
 */
              type_pcrit = NCL_float;
              rpcrit  = (float *)calloc(1,sizeof(float));
              *rpcrit = (float)(*pcrit);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'pcrit' attribute must be of type numeric. Defaulting to 50.");
              return_pcrit = False;
            }
          }
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "return_eval")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_eval = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'return_eval' attribute must be a logical. Defaulting to False.");
            }
          }
/*
 * Check for "return_trace".
 */
          if (!strcmp(attr_list->attname, "return_trace")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_trace = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'return_trace' attribute must be a logical. Defaulting to False.");
            }
          }
/*
 * Check for "anomalies".
 */
          if (!strcmp(attr_list->attname, "anomalies")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              anomalies = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'anomalies' attribute must be a logical. Will default to False");
            }
          }
/*
 * Check for "transpose".
 */
          if (!strcmp(attr_list->attname, "transpose")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              use_new_transpose = *(logical*) attr_list->attvalue->multidval.val;
              tr_setbyuser = True;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'transpose' attribute must be a logical. Will let routine pick best value.");
            }
          }
/*
 * Check for "oldtranspose".
 */
          if (!strcmp(attr_list->attname, "oldtranspose")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              use_old_transpose = *(logical*) attr_list->attvalue->multidval.val;
              tr_setbyuser = True;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'oldtranspose' attribute must be a logical. Will default to False.");
            }
          }
/*
 * Check for "debug".
 */
          if (!strcmp(attr_list->attname, "debug")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              debug = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The 'debug' attribute must be a logical. Defaulting to False.");
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
 * If user didn't set pcrit, then set it here.
 */
  if(!return_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    if( pcrit == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for pcrit");
      return(NhlFATAL);
    }
    *pcrit = 50.;
  }
  if(debug) {
    printf("eofunc_n: pcrit = %g\n", *pcrit);
    if(!anomalies) {
      printf("anomalies being removed...\n");
    }
    else {
      printf("anomalies NOT being removed...\n");
    }
  }

/*
 * Create arrays to store non-missing data and to remove mean from
 * data before entering Fortran routines.
 */
  dx_strip = (double *)calloc(nrow * ncol,sizeof(double));
  xave     = (double *)calloc(ncol,sizeof(double));
  xvar     = (double *)calloc(ncol,sizeof(double));
  xdvar    = (double *)calloc(ncol,sizeof(double));
  if( dx_strip == NULL || xave == NULL || xvar == NULL || xdvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

/*
 * Strip all grid points that have less than "PCRIT" valid values.
 * Create "dx_strip". This may have fewer columns/grid-pts
 * than the original "dx" array, if not all columns 
 * had the minimum number of valid values.
 */
  mcsta = 0;

  for( nc = 0; nc < ncol; nc++) {
/*
 * Statistics for this station/grid-point
 */
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&inrow,&missing_dx.doubleval,
                           &xave[nc],&xvar[nc],&xsd,&kntx,&ier);
/*
 * Eliminate stations/grid-points with less than pcrit % of data.
 */
    pcx = ((double)kntx/(double)nrow)*100.;
    if (pcx < *pcrit || xsd <= 0.0) {
      xave[nc] = missing_dx.doubleval;
    }
/* 
 * Create anomalies. If jopt=1, then normalize the anomalies.
 * mcsta is the number of acceptable grid/station points (mcsta <= msta).
 */
    con = 1.0;
    if(jopt == 1 && xave[nc] != missing_dx.doubleval && xsd > 0.0) {
      con = 1./xsd;
    }     
/*
 * Work with anomalies: xdave=0.0 [or standardized anomalies]
 */
    if (xave[nc] != missing_dx.doubleval) {
/*
 * The following can produce too much output, so I've commented it out.
 *
 *      if(debug) {
 *          printf("nc = %d xave = %g\n", nc, xave[nc]);
 *      }
 */
/*
 * Increment counter for acceptable points.
 */
      for( nr = 0; nr < nobs; nr++) {
        if(dx[nc*nrow+nr] != missing_dx.doubleval) {
          if(!anomalies) {
/*
 * User hasn't removed anomalies, so do it here.
 */
            dx_strip[mcsta*nrow+nr] = (dx[nc*nrow+nr] - xave[nc]) * con;
          }
          else {
            if(debug) {
              printf("anomalies NOT being removed...\n");
            }
/*
 * User has already removed anomalies, so leave alone.
 */
            dx_strip[mcsta*nrow+nr] = dx[nc*nrow+nr];
          }
        }
        else {
          dx_strip[mcsta*nrow+nr] = missing_dx.doubleval;
        }
      }
      if(jopt == 0) {
        xdvar[mcsta] = xvar[nc];
      }
      else {
        xdvar[mcsta] = 1.0;
      }
      mcsta++;
    }
  }

  if(mcsta > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imcsta = (int) mcsta;

/*
 * Depending on the size of the rightmost 2D arrays being processed, and/or
 * the value of the transpose or oldtranspose attributes, we call one of
 * three different Fortran routines. These routines basically behave the
 * same, except two of them operate on a transposed version of the 2d array.
 */
  if(debug) {
    printf("eofunc_n: msta = %ld mcsta = %ld nobs = %ld\n", msta, mcsta, nobs);
  }
/*
 * If one of the transpose attributes has not explicitly been set by the
 * user, then based on the sizes of the input array's dimensions, determine 
 * whether to call a transpose routine or not.
 */
  if(!tr_setbyuser) {
/*
 * If mcsta <= nrow, don't call transpose routine.
 */
    if(mcsta <= nrow) {
      use_new_transpose = False;    /* already the default */
      use_old_transpose = False;
      if(debug) {
        printf("eofunc_n: transpose set to False\n");
      }
    }
    else {
/*
 * Since mcsta > nrow, call transpose routine. 
 */
      use_new_transpose = True;
      use_old_transpose = False;
      if(debug) {
          printf("eofunc_n: transpose set to True\n");
      }
    }
  }
  else if(debug) {
/*
 * User explicitly set one of the transpose attributes, so indicate
 * which one here. Note that if both oldtranspose and transpose are
 * set to True, transpose will take precedence.
 */
    if(use_new_transpose) {
      printf("eofunc_n: user set use_new_transpose to True\n");
    }
    else if(use_old_transpose) { 
      printf("eofunc_n: user set use_old_transpose to True\n");
    }
    else {
      printf("eofunc_n: user set neither transpose attribute to True\n");
    }
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i < *dim; i++ )          dsizes_evec[i+1] = dsizes_x[i];
  for( i  = *dim+1; i < ndims_x; i++ ) dsizes_evec[i]   = dsizes_x[i];
  total_size_evec = *neval * ncol;

/*
 * Allocate memory for various arrays.  Depending on which Fortran routine
 * will be called later, different quantities need to be allocated here.
 */
  if(use_new_transpose) {
    xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
    wevec  = (double *)calloc(*neval * mcsta,sizeof(double));
    prncmp = (double *)calloc(*neval*nrow,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    pcvar  = (double *)calloc(*neval,sizeof(double));
    if(xdatat == NULL || wevec == NULL || prncmp == NULL || 
       eval == NULL || pcvar == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
/*
 * Determine whether the return eigenvectors will be float or double,
 * and allocate space if necessary.
 */
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
/*
 * If mcsta = msta, then we can use wevec as is. Otherwise, later we
 * need to copy wevec to locations in which the input was not missing.
 */
      if(mcsta != msta) {
        evec = (double*)calloc(total_size_evec,sizeof(double));
        if( evec == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for output array");
          return(NhlFATAL);
        }
      }
    }
/*
 * Transpose the input array.
 */
    l1=0;
    for(i = 0; i < mcsta; i++ ) {
      l2 = i;  
      for(j = 0; j < nrow; j++ ) {
        xdatat[l2] = dx_strip[l1];
        l1++;
        l2+=mcsta;

      }
    }
/*
 * Initialization for other arrays.
 */
    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      pcvar[ne] = eval[ne] = missing_dx.doubleval;
      for( nc = 0; nc < mcsta; nc++) {
        wevec[i] = missing_dx.doubleval;
        i++;
      }
    }
  }
  else if(use_old_transpose) {
    trace  = (double *)calloc(1,sizeof(double));
    evec   = (double*)calloc(total_size_evec,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    pcvar  = (double *)calloc(*neval,sizeof(double));
    xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
    if(trace == NULL || pcvar == NULL || eval == NULL || xdatat == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
/*
 * Initialization.
 */
    *trace = missing_dx.doubleval;
    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      pcvar[ne] = eval[ne] = missing_dx.doubleval;
      for( nc = 0; nc < ncol; nc++) {
        evec[i] = missing_dx.doubleval;
        i++;
      }
    }
  }
  else {
/*
 * eofcov routine
 *
 * Allocate space needed for various arrays.
 */
    wevec  = (double *)calloc(total_size_evec,sizeof(double));
    trace  = (double *)calloc(1,sizeof(double));
    eval   = (double *)calloc(*neval,sizeof(double));
    rpcvar = (float *)calloc(*neval,sizeof(float));
    if(wevec == NULL || trace == NULL || rpcvar == NULL || eval == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for various arrays");
      return(NhlFATAL);
    }
    if(type_x != NCL_double) {
      revec = (float*)calloc(total_size_evec,sizeof(float));
      if( revec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
    else {
/*
 * If mcsta = msta, then we can use wevec as is. Otherwise, later we
 * need to copy wevec to locations in which the input was not missing.
 */
      if(mcsta != msta) {
        evec = (double*)calloc(total_size_evec,sizeof(double));
        if( evec == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for output array");
          return(NhlFATAL);
        }
      }
    }

/*
 * Check sizes of work arrays that need to be passed to Fortran 
 * routine below.
 */
    llcssm = mcsta*(mcsta+1)/2;
    lwork  = 8*mcsta;
    liwork = 5*mcsta;
    lifail = mcsta;

    if((lwork > INT_MAX) || (liwork > INT_MAX) || (lifail > INT_MAX)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: one or more dimension sizes is greater than INT_MAX");
      return(NhlFATAL);
    }
    ilwork  = (int) lwork; 
    iliwork = (int) liwork; 
    ilifail = (int) lifail; 

/*
 * Initialization.
 */
    *trace = missing_dx.doubleval;

    i = 0;
    for( ne = 0; ne < *neval; ne++ ) {
      eval[ne]   = missing_dx.doubleval;
      rpcvar[ne] = (float)missing_dx.doubleval;
      for( nc = 0; nc < ncol; nc++) {
        wevec[i] = missing_dx.doubleval;
        i++;
      }
    }
/*
 * Create some work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
    lweval = lifail;
    cssm   = (double *)calloc(llcssm,sizeof(double));
    work   = (double *)calloc(lwork,sizeof(double));
    weval  = (double *)calloc(lweval,sizeof(double));
    iwork  =    (int *)calloc(liwork,sizeof(int));
    ifail  =    (int *)calloc(lifail,sizeof(int));
    if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL ||
        ifail == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for work arrays");
      return(NhlFATAL);
    }
  }

/*
 * Call the Fortran 77 version of appropriate routine.
 */
  if(use_new_transpose) {
    icovcor = 0;
    NGCALLF(deof11,DEOF11)(xdatat,&imcsta,&inrow,neval,&icovcor,
                           &missing_dx.doubleval,eval,wevec,pcvar,prncmp);
  }
  else if(use_old_transpose) {
    NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&inrow,&incol,&inobs,&imcsta,
                             &missing_dx.doubleval,neval,eval,evec,
                             pcvar,trace,xdvar,xave,&jopt,&ier);
  }
  else {
    NGCALLF(ddrveof,DDRVEOF)(dx_strip,&inrow,&incol,&inobs,&imcsta,
                             &missing_dx.doubleval,neval,eval,wevec,rpcvar,
                             trace,&iopt,&jopt,cssm,&llcssm,work,&ilwork,
                             weval,iwork,&iliwork,ifail,&ilifail,&ier);
  }
/*
 * If we used the "old" transpose routine, then the returned eigenvectors
 * have already been returned to the original-sized array with all the
 * missing values in the correct locations.  All we need to do here is 
 * convert to float if necessary.
 */
  if(use_old_transpose) {
    if(type_x != NCL_double) {
      for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];
/*
 * Free up double precision array.
 */
      NclFree(evec);
    }
  }
/*
 * If we are dealing with the old eofcov routine, or the new SCRIPPS
 * routine, then we need to reshape the evec (or revec if float)
 * array.  Note  that for the old eofcov routine, wevec is actually
 * the same size as evec, whereas for the new routine, it's the same 
 * size only if mcsta == msta. 
 */
  else {
    if(mcsta < msta) {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 *
 * This is the floating point (single precision) case.
 */
      if(type_x != NCL_double) {
        for(i = 0; i < total_size_evec; i++) {
          revec[i] = (float)missing_dx.doubleval;
        }
/*
 * Now copy over the appropriate values in the wevec array. Since the
 * wevec array is a different size depending on which routine you are
 * using, we have two different sections of code here.
 */
        if(use_new_transpose) {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                revec[ne*ncol+nc] = (float)wevec[ne*mcsta+nc2];
              }
              nc2++;
            }
          }
        }
        else {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                revec[ne*ncol+nc] = (float)wevec[ne*ncol+nc2];
              }
              nc2++;
            }
          }
        }
      }
/*
 * This is the double precision case.
 */
      else {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 */
        for(i = 0; i < total_size_evec; i++) {
          evec[i] = missing_dx.doubleval;
        }
/*
 * Now copy over the appropriate values in the wevec array. Since the
 * wevec array is a different size depending on which routine you are
 * using, we have two different sections of code here.
 */
        if(use_new_transpose) { 
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                evec[ne*ncol+nc] = wevec[ne*mcsta+nc2];
              }
              nc2++;
            }
          }
        }
        else {
          nc2 = 0;
          for( nc = 0; nc < ncol; nc++) {
            if (xave[nc] != missing_dx.doubleval) {
              for( ne = 0; ne < *neval; ne++ ) {
                evec[ne*ncol+nc] = wevec[ne*ncol+nc2];
              }
              nc2++;
            }
          }
        }
      }
      NclFree(wevec);
    }
    else {
/*
 * mcsta = msta, so we just need to copy stuff over. It doesn't matter
 * whether we have called the old eofcov routine or the new eof SCRIPPS
 * routine, because if mcsta==msta, then wevec is the same size for
 * both routines.
 */
      if(type_x != NCL_double) {
        for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)wevec[i];
        NclFree(wevec);
      }
      else {
        evec = wevec;
      }
    }
  }

/*
 * Check various possible error messages. The new transpose routine doesn't
 * have an ier.
 */
  if (!use_new_transpose && ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_n: %d eigenvectors failed to converge",ier);
    }
  }

/*
 * Free unneeded memory.
 */
  NclFree(dx_orig);
  NclFree(dx);
  NclFree(dx_strip);
  NclFree(xave);
  NclFree(xvar);
  NclFree(xdvar);
  if(!use_new_transpose && !use_old_transpose) {
    NclFree(work);
    NclFree(cssm);
    NclFree(weval);
    NclFree(iwork);
    NclFree(ifail);
  }
  else {
    NclFree(xdatat);
    if(use_new_transpose) NclFree(prncmp);
  }

/*
 * This is the start of a rather large if-else statement. It is based
 * on whether you are returning floats or doubles. 
 */
  if(type_x != NCL_double) {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * Coerce eval to float.
 */
      reval = (float *)calloc(*neval,sizeof(float));
      if( reval == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for eigenvalue array");
        return(NhlFATAL);
      }
      for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * If we didn't use the SCRIPPS routine, then the eigenvalues
 * returned are okay as is. Otherwise, we have to apply a scale
 * factor and return both the original values and the scaled values.
 */
      if(use_new_transpose) {
        reval2 = (float *)calloc(*neval,sizeof(float));
        if( reval2 == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for eigenvalue array");
          return(NhlFATAL);
        }
        scale_factor = (mcsta-1)/(nrow-1);
        for( i = 0; i < *neval; i++ ) reval2[i] = scale_factor * reval[i];
/*
 * First return original eigenvalues as "eval_transpose".
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval_transpose",
                 att_md,
                 NULL
                 );
/*
 * Now return scaled eigenvalues as simply "eval".
 */
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval2,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
      else {
/*
 * We didn't call the tranpose routine, so we only need to return
 * one set of eigenvalues. 
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
/*
 * Free up original eval array, since we don't need it anymore.
 */
      NclFree(eval);
    }
/*
 * Only return the trace if the appropriate option has been set.
 * The new transpose routine doesn't return trace.
 */
    if(!use_new_transpose) {
      if(return_trace) {
/*
 * Coerce trace to float.
 */
        rtrace = (float *)calloc(1,sizeof(float));
        *rtrace = (float)(*trace);
        dsizes[0] = 1;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rtrace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
        _NclAddAtt(
                   att_id,
                   "trace",
                   att_md,
                   NULL
                   );
      }
      NclFree(trace);
    }
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * If we didn't use the SCRIPPS routine, then the eigenvalues
 * returned are okay as is. Otherwise, we have to apply a scale
 * factor and return both the original values and the scaled values.
 */
      if(use_new_transpose) {
        eval2 = (double *)calloc(*neval,sizeof(double));
        if( eval2 == NULL ) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_n: Unable to allocate memory for eigenvalue array");
          return(NhlFATAL);
        }
        scale_factor = (mcsta-1)/(nrow-1);
        for( i = 0; i < *neval; i++ ) eval2[i] = scale_factor * eval[i];
/*
 * First return original eigenvalues as "eval_transpose".
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval_transpose",
                 att_md,
                 NULL
                 );
/*
 * Now return scaled eigenvalues as simply "eval".
 */
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval2,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
      }
      else {
/*
 * We didn't call the tranpose routine, so we only need to return
 * one set of eigenvalues. 
 */
        dsizes[0] = *neval;
        att_md = _NclCreateVal(
                               NULL,
                               NULL,
                               Ncl_MultiDValData,
                               0,
                               (void*)eval,
                               NULL,
                               1,
                               dsizes,
                               TEMPORARY,
                               NULL,
                               (NclObjClass)nclTypedoubleClass
                               );
        _NclAddAtt(
                   att_id,
                   "eval",
                   att_md,
                   NULL
                   );
      }
    }

    if(!use_new_transpose) {
      if(return_trace) {
        dsizes[0] = 1;  
        att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)trace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
        _NclAddAtt(
                   att_id,
                   "trace",
                   att_md,
                   NULL
                   );
      }
      else {
        NclFree(trace);
      }
    }
  }

/*
 * Return pcvar as float no matter what.
 */
  if(use_old_transpose || use_new_transpose) {
    rpcvar = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) rpcvar[i] = (float)pcvar[i];
    NclFree(pcvar);
  }
  dsizes[0] = *neval;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rpcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar",
             att_md,
             NULL
             );
/*
 * Only return "pcrit" if it was set by the user and we called one
 * of the transpose routines. The type returned is a float or a double,
 * depending on what pcrit was set to in the input.
 */
  if((use_new_transpose || use_old_transpose) && return_pcrit) {
    dsizes[0] = 1;
    if(type_pcrit == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rpcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)pcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
    }
    _NclAddAtt(
               att_id,
               "pcrit",
               att_md,
               NULL
               );
  }
  
/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
             att_md,
             NULL
             );

/*
 * "method" indicates whether the transpose routine was called or not.
 */
  if(use_new_transpose) {
    cmethod = (char *)calloc(10,sizeof(char));
    strcpy(cmethod,"transpose");
  }
  else if(use_old_transpose) {
    cmethod = (char *)calloc(14,sizeof(char));
    strcpy(cmethod,"old_transpose");
  }
  else {
    cmethod = (char *)calloc(13,sizeof(char));
    strcpy(cmethod,"no transpose");
  }
  method  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *method = NrmStringToQuark(cmethod);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)method,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "method",
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
 * Free memory 
 */
  NclFree(cmatrix);
  NclFree(cmethod);
  if((return_pcrit && type_pcrit != NCL_double) || !return_pcrit)  {
    NclFree(pcrit); 
  }

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);

  return(NhlNOERROR);
}


NhlErrorTypes eofunc_ts_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *devec;
  logical *opt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  ng_size_t nrow, ncol, nobs, msta, neval, ntime, total_size_x, total_size_evec;
  int inrow, incol, inobs, imsta, ineval;
  int iflag = 0, jopt = 0;
  ng_size_t i;
  int ier = 0;

/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  ng_size_t dsizes_evec_ts[2];
/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix;
  NclQuark *matrix;


/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           3,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);

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
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: All but the rightmost dimension of the first input array must be the same as all but the leftmost dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x/evec to double if necessary.
 */
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,&missing_devec);
  if(dx == NULL || devec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variables.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  evtsav  = (double *)calloc(neval,sizeof(double));
  if( evec_ts == NULL || evtsav == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
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
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "jopt"
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
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
 * Call the appropriate Fortran 77 routine.
 */
  NGCALLF(deofts7,DEOFTS7)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,&ineval,devec,&jopt,
                           &iflag,wx,wrk,evec_ts,evtsav,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) { 
       NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)devec != evec) NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    revtsav  = (float *)calloc(neval,sizeof(float));
    if( revec_ts == NULL || revtsav == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
    for( i = 0; i < neval; i++ )       revtsav[i]  = (float)evtsav[i];
/*
 * Free up double precision arrays.
 */
    NclFree(evec_ts);
    NclFree(evtsav);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec_ts,
                              &missing_rx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)revtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );

  }
  else {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec_ts,
                              &missing_dx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)evtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );
  }

/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
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
 * Free memory 
 */
  NclFree(cmatrix);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}

NhlErrorTypes eofunc_ts_n_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *dx_orig, *devec;
  logical *opt;
  int *dim;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  ng_size_t nrow, ncol, nobs, msta, neval, ntime, total_size_x, total_size_evec;
  ng_size_t i, nr, nl, nm, counter, ireordered;
  ng_size_t size_leftmost, size_middle, size_rightmost, size_middle_rightmost;
  ng_size_t left_loc, mid_loc;
  int inrow, incol, inobs, imsta, ineval;
  int iflag = 0, jopt = 0;
  int ier = 0;

/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  ng_size_t dsizes_evec_ts[2];
/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix;
  NclQuark *matrix;


/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           4,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           4,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);

  opt = (logical*)NclGetArgValue(
           2,
           4,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

 /*
  * Retrieve the dimension index for the "time" dimension.
  */ 
  dim = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Make sure "dim" is a valid dimension.
 */
  if (*dim < 0 || *dim >= ndims_x) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: Invalid dimension index for 'time'");
    return(NhlFATAL);
  }


/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  for( i = 0; i < *dim; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: All but the 'time' dimension of the first input array must be the same as all but the leftmost dimension of the second input array");
      return(NhlFATAL);
    }
  }
  for( i = *dim+1; i < ndims_x; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: All but the 'time' dimension of the first input array must be the same as all but the leftmost dimension of the second input array");
      return(NhlFATAL);
    }
  }

/*
 * Calculate the left, middle and right dimensions, so we
 * can reorder array. The other ncol, ncor, nobs variables
 * are a bit confusing, but they are used as various 
 * arguments to the Fortran eof routine.
 * 
 * The important thing here is that size_leftmost * size_middle * size_rightmost
 * should equal the total size of x (total_size_x). Also, size_middle
 * is the "time" dimension that is specified by the "dim" argument 
 * above.
 * 
 *
 */
  size_rightmost = size_leftmost = 1;
  for( i = 0; i < *dim; i++ ) size_leftmost *= dsizes_x[i];
  for( i = *dim+1; i < ndims_x; i++ ) size_rightmost *= dsizes_x[i];
  ncol = msta = size_rightmost * size_leftmost;
  size_middle = nobs = nrow = ntime = dsizes_x[*dim];
  size_middle_rightmost = size_rightmost * size_middle;
 
  neval           = dsizes_evec[0];
  total_size_x    = ncol * nrow;
  total_size_evec = ncol * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }

  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x/evec to double. Note:  coerce_input_double will return a pointer to
 * the original data if it is already double, which is what we want for 
 * devec.  coerce_subset_input_double will force the coercion no matter 
 * what, which we need for x because we may have to reorder it.
 */
  dx_orig = (double*)malloc(total_size_x*sizeof(double));
  dx      = (double*)malloc(total_size_x*sizeof(double));
  devec   = coerce_input_double(evec,type_evec,total_size_evec,
                                has_missing_evec,&missing_evec,&missing_devec);
  if(dx_orig == NULL || dx == NULL || devec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
  coerce_subset_input_double(x,dx_orig,0,type_x,total_size_x,0,NULL,NULL);

/*
 * Create a vector containing the reordered indices, and also
 * copy reordered data into new array.
 */
  counter = 0;
  for(nl = 0; nl < size_leftmost; nl++) {
    left_loc = nl * size_middle_rightmost;
    for(nr = 0; nr < size_rightmost; nr++) {
      for(nm = 0; nm < size_middle; nm++) {
        mid_loc = nm * size_rightmost;
        ireordered = left_loc + mid_loc + nr;
        ((double*)dx)[counter] = ((double*)dx_orig)[ireordered];
        counter++;
      }
    }
  }

/*
 * Allocate memory for return variables.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  evtsav  = (double *)calloc(neval,sizeof(double));
  if( evec_ts == NULL || evtsav == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }

/* 
 * If "opt" is True, then check if any attributes have been set.
 */
  if(*opt) {
    stack_entry = _NclGetArg(2, 4, DONT_CARE);
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
 *
 *   "jopt"
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
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
 * Call the appropriate Fortran 77 routine.
 */
  NGCALLF(deofts7,DEOFTS7)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,&ineval,devec,&jopt,
                           &iflag,wx,wrk,evec_ts,evtsav,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) { 
       NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofunc_ts_n: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  NclFree(dx_orig);
  NclFree(dx);
  if((void*)devec != evec) NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    revtsav  = (float *)calloc(neval,sizeof(float));
    if( revec_ts == NULL || revtsav == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts_n: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
    for( i = 0; i < neval; i++ )       revtsav[i]  = (float)evtsav[i];
/*
 * Free up double precision arrays.
 */
    NclFree(evec_ts);
    NclFree(evtsav);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec_ts,
                              &missing_rx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)revtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );

  }
  else {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec_ts,
                              &missing_dx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)evtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );
  }

/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
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
 * Free memory 
 */
  NclFree(cmatrix);

/*
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);
  return(NhlNOERROR);
}

NhlErrorTypes eofcov_tr_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  logical *opt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  ng_size_t nrow, ncol, nobs, msta, mcsta, nc, nc2, nr;
  int inrow, imcsta, kntx;
  ng_size_t total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  float  *rpcvar;
  double *pcrit = NULL;
  float *rpcrit = NULL;
  NclBasicDataTypes type_pcrit = NCL_none;
  ng_size_t i, j, l1, l2;
  int jopt = 0, ier = 0, icovcor;
  logical anomalies = False, debug = False;
  logical return_eval = True, return_pcrit = False;
/*
 * Work array variables.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;

/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix, *cmethod;
  NclQuark *matrix, *method;
  double *eval, *pcvar, *prncmp;
  float *reval;
/*
 * Output array variables
 */
  double *evec = NULL;
  double *wevec = NULL;
  double *xdatat;
  float *revec = NULL;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * Get option.
 */
  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow = (int) nrow;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/* 
 * If "opt" is True, then check if any attributes have been set.
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
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "jopt"        : both routines
 *   "return_eval" : both routines (unadvertised)
 *   "return_pcrit": return pcrit
 *   "pcrit"       : transpose routine only
 *   "anomalies"   : If True, anomalies have already been calculated by
 *                   user, and this interface shouldn't remove them.
 *   "debug"       : turn on debug
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
            }
          }
/*
 * Check for "pcrit". If user sets this attribute, then we'll return
 * it as an attribute of the return variable.
 */
          if(!strcmp(attr_list->attname, "pcrit")) {
            type_pcrit   = attr_list->attvalue->multidval.data_type;
            return_pcrit = True;
/*
 * If "pcrit" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_pcrit == NCL_double) {
              pcrit  = (double *)calloc(1,sizeof(double));
              *pcrit = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_pcrit == NCL_int || type_pcrit == NCL_float) {
/*
 * Coerce to float, if it's not double.
 */
              pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_pcrit,1,0,NULL,NULL);
/*
 * For later, when we return "pcrit" as an attribute of the return value.
 */
              type_pcrit = NCL_float;
              rpcrit  = (float *)calloc(1,sizeof(float));
              *rpcrit = (float)(*pcrit);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'pcrit' attribute must be of type numeric. Defaulting to 50.");
              return_pcrit = False;
            }
          }
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "return_eval")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_eval = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'return_eval' attribute must be a logical. Defaulting to False.");
            }
          }
/*
 * Check for "anomalies".
 */
          if (!strcmp(attr_list->attname, "anomalies")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              anomalies = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'anomalies' attribute must be a logical. Will default to False");
            }
          }
/*
 * Check for "debug".
 */
          if (!strcmp(attr_list->attname, "debug")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              debug = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr: The 'debug' attribute must be a logical. Defaulting to False.");
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
 * If user didn't set pcrit, then set it here.
 */
  if(!return_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    if( pcrit == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for pcrit");
      return(NhlFATAL);
    }
    *pcrit = 50.;
  }
  if(debug) {
    printf("eofcov_tr: pcrit = %g\n", *pcrit);
    if(!anomalies) {
      printf("anomalies being removed...\n");
    }
    else {
      printf("anomalies NOT being removed...\n");
    }
  }

/*
 * Create arrays to store non-missing data and to remove mean from
 * data before entering Fortran routines.
 */
  dx_strip = (double *)calloc(nrow * ncol,sizeof(double));
  xave     = (double *)calloc(ncol,sizeof(double));
  xvar     = (double *)calloc(ncol,sizeof(double));
  xdvar    = (double *)calloc(ncol,sizeof(double));
  if( dx_strip == NULL || xave == NULL || xvar == NULL || xdvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

/*
 * Strip all grid points that have less that "PCRIT" valid values.
 * Create "dx_strip". This may have fewer columns/grid-pts
 * than the original "dx" array, if not all columns 
 * had the minimun number of valid values.
 */
  mcsta = 0;

  for( nc = 0; nc < ncol; nc++) {
/*
 * Statistics for this station/grid-point
 */
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&inrow,&missing_dx.doubleval,
                           &xave[nc],&xvar[nc],&xsd,&kntx,&ier);
/*
 * Eliminate stations/grid-points with less than pcrit % of data.
 */
    pcx = ((double)kntx/(double)nrow)*100.;
    if (pcx < *pcrit || xsd <= 0.0) {
      xave[nc] = missing_dx.doubleval;
    }
/* 
 * Create anomalies. If jopt=1, then normalize the anomalies.
 * mcsta is the number of acceptable grid/station points (mcsta <= msta).
 */
    con = 1.0;
    if(jopt == 1 && xave[nc] != missing_dx.doubleval && xsd > 0.0) {
      con = 1./xsd;
    }     
/*
 * Work with anomalies: xdave=0.0 [or standardized anomalies]
 */
    if (xave[nc] != missing_dx.doubleval) {
/*
 * The following can produce too much output, so I've commented it out.
 *
 *      if(debug) {
 *          printf("nc = %ld xave = %g\n", nc, xave[nc]);
 *      }
 */
/*
 * Increment counter for acceptable points.
 */
      for( nr = 0; nr < nobs; nr++) {
        if(dx[nc*nrow+nr] != missing_dx.doubleval) {
          if(!anomalies) {
/*
 * User hasn't removed anomalies, so do it here.
 */
            dx_strip[mcsta*nrow+nr] = (dx[nc*nrow+nr] - xave[nc]) * con;
          }
          else {
            if(debug) {
              printf("anomalies NOT being removed...\n");
            }
/*
 * User has already removed anomalies, so leave alone.
 */
            dx_strip[mcsta*nrow+nr] = dx[nc*nrow+nr];
          }
        }
        else {
          dx_strip[mcsta*nrow+nr] = missing_dx.doubleval;
        }
      }
      if(jopt == 0) {
        xdvar[mcsta] = xvar[nc];
      }
      else {
        xdvar[mcsta] = 1.0;
      }
      mcsta++;
    }
  }

  if(mcsta > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imcsta = (int) mcsta;

  if(debug) {
    printf("eofcov_tr: msta = %ld mcsta = %ld nobs = %ld\n", msta, mcsta, nobs);
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];
  total_size_evec = *neval * ncol;

/*
 * Allocate memory for various arrays.
 */
  xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
  wevec  = (double *)calloc(*neval * mcsta,sizeof(double));
  prncmp = (double *)calloc(*neval*nrow,sizeof(double));
  eval   = (double *)calloc(*neval,sizeof(double));
  pcvar  = (double *)calloc(*neval,sizeof(double));
  if(xdatat == NULL || wevec == NULL || prncmp == NULL || 
     eval == NULL || pcvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for various arrays");
    return(NhlFATAL);
  }
/*
 * Determine whether the return eigenvectors will be float or double,
 * and allocate space if necessary.
 */
  if(type_x != NCL_double) {
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
  }
  else {
/*
 * If mcsta = msta, then we can use wevec as is. Otherwise, later we
 * need to copy wevec to locations in which the input was not missing.
 */
    if(mcsta != msta) {
      evec = (double*)calloc(total_size_evec,sizeof(double));
      if( evec == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for output array");
        return(NhlFATAL);
      }
    }
  }
/*
 * Transpose the input array.
 */
  l1=0;
  for(i = 0; i < mcsta; i++ ) {
    l2 = i;  
    for(j = 0; j < nrow; j++ ) {
      xdatat[l2] = dx_strip[l1];
      l1++;
      l2+=mcsta;
      
    }
  }
/*
 * Initialization for other arrays.
 */
  i = 0;
  for( ne = 0; ne < *neval; ne++ ) {
    pcvar[ne] = eval[ne] = missing_dx.doubleval;
    for( nc = 0; nc < mcsta; nc++) {
      wevec[i] = missing_dx.doubleval;
      i++;
    }
  }

/*
 * Call the Fortran 77 version of appropriate routine.
 */
  icovcor = 0;
  NGCALLF(deof11,DEOF11)(xdatat,&imcsta,&inrow,neval,&icovcor,
                         &missing_dx.doubleval,eval,wevec,pcvar,prncmp);
/*
 * If mcsta < msta then we need to "fix" the evec (or revec if float)
 * array.
 */
  if(mcsta < msta) {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 *
 * This is the floating point (single precision) case.
 */
    if(type_x != NCL_double) {
      for(i = 0; i < total_size_evec; i++) {
        revec[i] = (float)missing_dx.doubleval;
      }
/*
 * Now copy over the appropriate values in the wevec array.
 */
      nc2 = 0;
      for( nc = 0; nc < ncol; nc++) {
        if (xave[nc] != missing_dx.doubleval) {
          for( ne = 0; ne < *neval; ne++ ) {
            revec[ne*ncol+nc] = (float)wevec[ne*mcsta+nc2];
          }
          nc2++;
        }
      }
    }
    else {
/*
 * First, make sure init to missing because not all values will be 
 * filled in.
 */
      for(i = 0; i < total_size_evec; i++) {
        evec[i] = missing_dx.doubleval;
      }
/*
 * Now copy over the appropriate values in the wevec array.
 */
      nc2 = 0;
      for( nc = 0; nc < ncol; nc++) {
        if (xave[nc] != missing_dx.doubleval) {
          for( ne = 0; ne < *neval; ne++ ) {
            evec[ne*ncol+nc] = wevec[ne*mcsta+nc2];
          }
          nc2++;
        }
      }
    }
    NclFree(wevec);
  }
  else {
/*
 * mcsta = msta, so we just need to copy stuff over.
 */
    if(type_x != NCL_double) {
      for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)wevec[i];
      NclFree(wevec);
    }
    else {
      evec = wevec;
    }
  }

/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(dx_strip);
  NclFree(xave);
  NclFree(xvar);
  NclFree(xdvar);
  NclFree(xdatat);
  NclFree(prncmp);

  if(type_x != NCL_double) {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * Coerce eval to float.
 */
      reval = (float *)calloc(*neval,sizeof(float));
      for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
      NclFree(eval);

      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
  }
/*
 * Return pcvar as float no matter what.
 */

  rpcvar = (float *)calloc(*neval,sizeof(float));
  for( i = 0; i < *neval; i++ ) rpcvar[i] = (float)pcvar[i];
  NclFree(pcvar);

  dsizes[0] = *neval;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)rpcvar,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypefloatClass
                         );
  _NclAddAtt(
             att_id,
             "pcvar",
             att_md,
             NULL
             );
/*
 * Only return "pcrit" if it was set by the user.
 * The type returned is a float or a double,
 * depending on what pcrit was set to in the input.
 */
  if(return_pcrit) {
    dsizes[0] = 1;
    if(type_pcrit == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rpcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)pcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
    }
    _NclAddAtt(
               att_id,
               "pcrit",
               att_md,
               NULL
               );
  }
  
/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
             att_md,
             NULL
             );

/*
 * "method" indicates whether the transpose routine was called or not.
 */
  cmethod = (char *)calloc(10,sizeof(char));
  strcpy(cmethod,"transpose");
  method  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *method = NrmStringToQuark(cmethod);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)method,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "method",
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


NhlErrorTypes eofcov_tr_old_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  logical *opt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int inrow, incol, inobs, imcsta, kntx;
  ng_size_t nrow, ncol, nobs, msta, mcsta, nc, nr;
  ng_size_t total_size_x;
  int *neval;
/*
 * Various.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *xdatat;
  double *pcrit = NULL;
  float *rpcrit = NULL;
  NclBasicDataTypes type_pcrit = NCL_none;
  int jopt = 0, ier = 0;
  ng_size_t i;
  logical return_trace = False, return_pcrit = False,  return_eval = False;
  logical debug = False;
/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix;
  NclQuark *matrix;
  double *trace, *eval, *pcvar;
  float *rtrace, *reval, *rpcvar;

/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * Get option.
 */
  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow = (int) nrow;
  incol = (int) ncol;
  inobs = (int) nobs;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar = (double *)calloc(*neval,sizeof(double));
  if( trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }

/*
 * Initialize to missing.
 */
  *trace = missing_dx.doubleval;

/* 
 * If "opt" is True, then check if any attributes have been set.
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
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "jopt"        : both routines
 *   "return_eval" : both routines (unadvertised)
 *   "pcrit"       : transpose routine only
 *   "debug"       : turn on debug
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The 'jopt' attribute must be an integer, defaulting to 0.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The 'jopt' attribute must be 0 or 1. Defaulting to 0.");
                jopt = 0;
              }
            }
          }
/*
 * Check for "pcrit". If user sets this attribute, then we'll return
 * it as an attribute of the return variable.
 */
          if(!strcmp(attr_list->attname, "pcrit")) {
            type_pcrit   = attr_list->attvalue->multidval.data_type;
            return_pcrit = True;
/*
 * If "pcrit" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_pcrit == NCL_double) {
              pcrit  = (double *)calloc(1,sizeof(double));
              *pcrit = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_pcrit == NCL_int || type_pcrit == NCL_float) {
/*
 * Coerce to float, if it's not double.
 */
              pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_pcrit,1,0,NULL,NULL);
/*
 * For later, when we return "pcrit" as an attribute of the return value.
 */
              type_pcrit = NCL_float;
              rpcrit  = (float *)calloc(1,sizeof(float));
              *rpcrit = (float)(*pcrit);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The 'pcrit' attribute must be of type numeric. Defaulting to 50.");
              return_pcrit = False;
            }
          }
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "return_eval")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_eval = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The 'return_eval' attribute must be a logical. Defaulting to True.");
            }
          }
/*
 * Check for "debug".
 */
          if (!strcmp(attr_list->attname, "debug")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              debug = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The 'debug' attribute must be a logical. Defaulting to False.");
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
 * If user didn't set pcrit, then set it here.
 */
  if(!return_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    if( pcrit == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for pcrit");
      return(NhlFATAL);
    }
    *pcrit = 50.;
  }
  if(debug) {
    printf("eofcov_tr_old: pcrit = %g\n", *pcrit);
  }


/*
 * Create arrays to store non-missing data and to remove mean from
 * data before entering Fortran routines.
 */
  dx_strip = (double *)calloc(nrow * ncol,sizeof(double));
  xave     = (double *)calloc(ncol,sizeof(double));
  xvar     = (double *)calloc(ncol,sizeof(double));
  xdvar    = (double *)calloc(ncol,sizeof(double));
  if( dx_strip == NULL || xave == NULL || xvar == NULL || xdvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

/*
 * Strip all grid points that have less that "PCRIT" valid values.
 * Create "dx_strip". This may have fewer columns/grid-pts
 * than the original "dx" array, if not all columns 
 * had the minimun number of valid values.
 */
  mcsta = 0;

  for( nc = 0; nc < ncol; nc++) {
/*
 * Statistics for this station/grid-point
 */
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&inrow,&missing_dx.doubleval,
                           &xave[nc],&xvar[nc],&xsd,&kntx,&ier);
/*
 * Eliminate stations/grid-oints with less than pcrit % of data.
 */
    pcx = ((double)kntx/(double)nrow)*100.;
    if (pcx < *pcrit || xsd <= 0.0) {
      xave[nc] = missing_dx.doubleval;
    }
/* 
 * Create anomalies. If jopt=1, then normalize the anomalies.
 * mcsta is the number of acceptable grid/station points (mcsta <= msta).
 */
    con = 1.0;
    if(jopt == 1 && xave[nc] != missing_dx.doubleval && xsd > 0.0) {
      con = 1./xsd;
    }     
/*
 * Work with anomalies: xdave=0.0 [or standardized anomalies]
 */
    if (xave[nc] != missing_dx.doubleval) {
/*
 * Increment counter for acceptable points.
 */
      for( nr = 0; nr < nobs; nr++) {
        if(dx[nc*nrow+nr] != missing_dx.doubleval) {
          dx_strip[mcsta*nrow+nr] = (dx[nc*nrow+nr] - xave[nc]) * con;
        }
        else {
          dx_strip[mcsta*nrow+nr] = missing_dx.doubleval;
        }
      }
      if(jopt == 0) {
        xdvar[mcsta] = xvar[nc];
      }
      else {
        xdvar[mcsta] = 1.0;
      }
      mcsta++;
    }
  }

  if(mcsta > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imcsta = (int) mcsta;

/*
 * We have to allocate this array separately, because it depends
 * on the value of "mcsta".
 */
  xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
  if(xdatat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

  NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&inrow,&incol,&inobs,&imcsta,
                           &missing_dx.doubleval,neval,eval,evec,
                           pcvar,trace,xdvar,xave,&jopt,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_tr_old: %d eigenvectors failed to converge",ier);
    }
  }

/*
 * Free unneeded memory common to both routines.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(dx_strip);
  NclFree(xdatat);
  NclFree(xave);
  NclFree(xvar);
  NclFree(xdvar);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * Coerce eval to float.
 */
      reval = (float *)calloc(*neval,sizeof(float));
      for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
      NclFree(eval);

      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
/*
 * Only return the trace if the appropriate option has been set.
 */
    if(return_trace) {
/*
 * Coerce trace to float.
 */
      rtrace = (float *)calloc(1,sizeof(float));
      *rtrace = (float)(*trace);
      dsizes[0] = 1;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rtrace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "trace",
                 att_md,
                 NULL
                 );
    }
/*
 * Coerce pcvar to float.
 */
    rpcvar = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) rpcvar[i] = (float)pcvar[i];
    NclFree(pcvar);
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );
  }
  else {

/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
  
    if(return_trace) {
      dsizes[0] = 1;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)trace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "trace",
                 att_md,
                 NULL
                 );
    }
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );
  }
/*
 * Only return "pcrit" if it was set by the user.
 * The type returned is a float or a double,
 * depending on what pcrit was set to in the input.
 */
  if(return_pcrit) {
    dsizes[0] = 1;
    if(type_pcrit == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rpcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)pcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
    }
    _NclAddAtt(
               att_id,
               "pcrit",
               att_md,
               NULL
               );
  }
  
/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
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


NhlErrorTypes eofcor_tr_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  logical *opt;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int inrow, incol, inobs, imcsta, kntx;
  ng_size_t nrow, ncol, msta, mcsta, nobs, nc, nr;
  ng_size_t total_size_x;
  int *neval;
/*
 * Various.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *xdatat;
  double *pcrit = NULL;
  float *rpcrit = NULL;
  NclBasicDataTypes type_pcrit = NCL_none;
  int jopt = 1, ier = 0;
  ng_size_t i;
  logical return_trace = False, return_pcrit = False,  return_eval = False;
  logical debug = False;
/*
 * Variables for retrieving attributes from "opt".
 */
  NclAttList  *attr_list;
  NclAtt  attr_obj;
  NclStackEntry   stack_entry;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix;
  NclQuark *matrix;
  double *trace, *eval, *pcvar;
  float *rtrace, *reval, *rpcvar;

/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * Get option.
 */
  opt = (logical *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow = (int) nrow;
  incol = (int) ncol;
  inobs = (int) nobs;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar = (double *)calloc(*neval,sizeof(double));
  if( trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }

/*
 * Initialize to missing.
 */
  *trace = missing_dx.doubleval;

/* 
 * If "opt" is True, then check if any attributes have been set.
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
 * Loop through attributes and check them. The current ones recognized are:
 *
 *   "jopt"        : both routines
 *   "return_eval" : both routines (unadvertised)
 *   "pcrit"       : transpose routine only
 *   "debug"       : turn on debug
 *
 */
        while (attr_list != NULL) {
/*
 * Check for "jopt".
 */
          if (!strcmp(attr_list->attname, "jopt")) {
            if(attr_list->attvalue->multidval.data_type != NCL_int) {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The 'jopt' attribute must be an integer, defaulting to 1.");
            }
            else {
              jopt = *(int*) attr_list->attvalue->multidval.val;
              if(jopt != 0 && jopt != 1) {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The 'jopt' attribute must be 0 or 1. Defaulting to 1.");
                jopt = 1;
              }
            }
          }
/*
 * Check for "pcrit". If user sets this attribute, then we'll return
 * it as an attribute of the return variable.
 */
          if(!strcmp(attr_list->attname, "pcrit")) {
            type_pcrit   = attr_list->attvalue->multidval.data_type;
            return_pcrit = True;
/*
 * If "pcrit" is already double, don't just point it to the attribute,
 * because we need to return it later.
 */
            if(type_pcrit == NCL_double) {
              pcrit  = (double *)calloc(1,sizeof(double));
              *pcrit = *(double*) attr_list->attvalue->multidval.val;
            }
            else if(type_pcrit == NCL_int || type_pcrit == NCL_float) {
/*
 * Coerce to float, if it's not double.
 */
              pcrit = coerce_input_double(attr_list->attvalue->multidval.val,
                                          type_pcrit,1,0,NULL,NULL);
/*
 * For later, when we return "pcrit" as an attribute of the return value.
 */
              type_pcrit = NCL_float;
              rpcrit  = (float *)calloc(1,sizeof(float));
              *rpcrit = (float)(*pcrit);
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The 'pcrit' attribute must be of type numeric. Defaulting to 50.");
              return_pcrit = False;
            }
          }
/*
 * Check for "return_eval".
 */
          if (!strcmp(attr_list->attname, "return_eval")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              return_eval = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The 'return_eval' attribute must be a logical. Defaulting to True.");
            }
          }
/*
 * Check for "debug".
 */
          if (!strcmp(attr_list->attname, "debug")) {
            if(attr_list->attvalue->multidval.data_type == NCL_logical) {
              debug = *(logical*) attr_list->attvalue->multidval.val;
            }
            else {
              NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The 'debug' attribute must be a logical. Defaulting to False.");
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
 * If user didn't set pcrit, then set it here.
 */
  if(!return_pcrit) {
    pcrit = (double *)calloc(1,sizeof(double));
    if( pcrit == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for pcrit");
      return(NhlFATAL);
    }
    *pcrit = 50.;
  }
  if(debug) {
    printf("eofcor_tr: pcrit = %g\n", *pcrit);
  }

 /*
 * Create arrays to store non-missing data and to remove mean from
 * data before entering Fortran routines.
 */
  dx_strip = (double *)calloc(nrow * ncol,sizeof(double));
  xave     = (double *)calloc(ncol,sizeof(double));
  xvar     = (double *)calloc(ncol,sizeof(double));
  xdvar    = (double *)calloc(ncol,sizeof(double));
  if( dx_strip == NULL || xave == NULL || xvar == NULL || xdvar == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

/*
 * Strip all grid points that have less that "PCRIT" valid values.
 * Create "dx_strip". This may have fewer columns/grid-pts
 * than the original "dx" array, if not all columns 
 * had the minimun number of valid values.
 */
  mcsta = 0;

  for( nc = 0; nc < ncol; nc++) {
/*
 * Statistics for this station/grid-point
 */
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&inrow,&missing_dx.doubleval,
                           &xave[nc],&xvar[nc],&xsd,&kntx,&ier);
/*
 * Eliminate stations/grid-oints with less than pcrit % of data.
 */
    pcx = ((double)kntx/(double)nrow)*100.;
    if (pcx < *pcrit || xsd <= 0.0) {
      xave[nc] = missing_dx.doubleval;
    }
/* 
 * Create anomalies. If jopt=1, then normalize the anomalies.
 * mcsta is the number of acceptable grid/station points (mcsta <= msta).
 */
    con = 1.0;
    if(jopt == 1 && xave[nc] != missing_dx.doubleval && xsd > 0.0) {
      con = 1./xsd;
    }     
/*
 * Work with anomalies: xdave=0.0 [or standardized anomalies]
 */
    if (xave[nc] != missing_dx.doubleval) {
/*
 * Increment counter for acceptable points.
 */
      for( nr = 0; nr < nobs; nr++) {
        if(dx[nc*nrow+nr] != missing_dx.doubleval) {
          dx_strip[mcsta*nrow+nr] = (dx[nc*nrow+nr] - xave[nc]) * con;
        }
        else {
          dx_strip[mcsta*nrow+nr] = missing_dx.doubleval;
        }
      }
      if(jopt == 0) {
        xdvar[mcsta] = xvar[nc];
      }
      else {
        xdvar[mcsta] = 1.0;
      }
      mcsta++;
    }
  }

  if(mcsta > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imcsta = (int) mcsta;

/*
 * We have to allocate this array separately, because it depends
 * on the value of "mcsta".
 */
  xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
  if(xdatat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

  NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&inrow,&incol,&inobs,&imcsta,
                           &missing_dx.doubleval,neval,eval,evec,
                           pcvar,trace,xdvar,xave,&jopt,&ier);

/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_tr: %d eigenvectors failed to converge",ier);
    }
  }

/*
 * Free unneeded memory common to both routines.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(dx_strip);
  NclFree(xdatat);
  NclFree(xave);
  NclFree(xvar);
  NclFree(xdvar);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
/*
 * Coerce eval to float.
 */
      reval = (float *)calloc(*neval,sizeof(float));
      for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
      NclFree(eval);

      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)reval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
/*
 * Only return the trace if the appropriate option has been set.
 */
    if(return_trace) {
/*
 * Coerce trace to float.
 */
      rtrace = (float *)calloc(1,sizeof(float));
      *rtrace = (float)(*trace);
      dsizes[0] = 1;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rtrace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
      _NclAddAtt(
                 att_id,
                 "trace",
                 att_md,
                 NULL
                 );
    }
/*
 * Coerce pcvar to float.
 */
    rpcvar = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) rpcvar[i] = (float)pcvar[i];
    NclFree(pcvar);
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );
  }
  else {

/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Only return the eigenvalues if the appropriate option has been set.
 */
    if(return_eval) {
      dsizes[0] = *neval;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)eval,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "eval",
                 att_md,
                 NULL
                 );
    }
  
    if(return_trace) {
      dsizes[0] = 1;
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)trace,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
      _NclAddAtt(
                 att_id,
                 "trace",
                 att_md,
                 NULL
                 );
    }
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );
  }
/*
 * Only return "pcrit" if it was set by the user.
 * The type returned is a float or a double,
 * depending on what pcrit was set to in the input.
 */
  if(return_pcrit) {
    dsizes[0] = 1;
    if(type_pcrit == NCL_float) {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)rpcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypefloatClass
                             );
    }
    else {
      att_md = _NclCreateVal(
                             NULL,
                             NULL,
                             Ncl_MultiDValData,
                             0,
                             (void*)pcrit,
                             NULL,
                             1,
                             dsizes,
                             TEMPORARY,
                             NULL,
                             (NclObjClass)nclTypedoubleClass
                             );
    }
    _NclAddAtt(
               att_id,
               "pcrit",
               att_md,
               NULL
               );
  }
  
/*
 * "matrix" indicates whether the covariance or correlation matrix
 * was used.
 */
  if(jopt == 0) {
    cmatrix = (char *)calloc(11,sizeof(char));
    strcpy(cmatrix,"covariance");
  }
  else {
    cmatrix = (char *)calloc(12,sizeof(char));
    strcpy(cmatrix,"correlation");
  }
  matrix  = (NclQuark*)NclMalloc(sizeof(NclQuark));
  *matrix = NrmStringToQuark(cmatrix);
  
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)matrix,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypestringClass
                         );
  _NclAddAtt(
             att_id,
             "matrix",
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


NhlErrorTypes eofcov_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int inrow, incol, inobs, imsta;
  ng_size_t nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 0;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  ng_size_t lwork, liwork, lifail;
  int ilwork, iliwork, ilifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
  double *trace, *eval;
  float *pcvar, *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol   = msta;
  nobs   = nrow = dsizes_x[ndims_x-1];
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (nobs > INT_MAX) || (lwork > INT_MAX) || (liwork > INT_MAX) || 
     (lifail > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow   = (int) nrow;
  incol   = (int) ncol;
  imsta   = (int) msta;
  inobs   = (int) nobs;
  ilwork  = (int) lwork;
  iliwork = (int) liwork;
  ilifail = (int) lifail;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar = (float *)calloc(*neval,sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  iwork  =   (int *)calloc(liwork,sizeof(int));
  ifail  =   (int *)calloc(lifail,sizeof(int));
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL ||
      ifail == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,cssm,&lcssm,work,&ilwork,
                           weval,iwork,&iliwork,ifail,&ilifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)reval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );

/*
 * pcvar is returned as float no matter what. 
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

/*
 * Coerce trace to float.
 */
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rtrace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                                                   );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)eval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );
/*
 * pcvar is returned as float no matter what.
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)trace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }

/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 0;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcor_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int inrow, incol, inobs, imsta;
  ng_size_t nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 1;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  ng_size_t lwork, liwork, lifail;
  int ilwork, iliwork, ilifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
  double *trace, *eval;
  float *pcvar, *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            2, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol   = msta;
  nobs   = nrow = dsizes_x[ndims_x-1];
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (nobs > INT_MAX) || (lwork > INT_MAX) || (liwork > INT_MAX) || 
     (lifail > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow   = (int) nrow;
  incol   = (int) ncol;
  imsta   = (int) msta;
  inobs   = (int) nobs;
  ilwork  = (int) lwork;
  iliwork = (int) liwork;
  ilifail = (int) lifail;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if( dx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for coercing x array to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar = (float *)calloc(*neval,sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  iwork  =   (int *)calloc(liwork,sizeof(int));
  ifail  =   (int *)calloc(lifail,sizeof(int));
  if( cssm == NULL || work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(ddrveof,DDRVEOF)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,cssm,&lcssm,work,&ilwork,
                           weval,iwork,&iliwork,ifail,&ilifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor: %d eigenvectors failed to converge.\nPoorly conditioned correlation matrix.",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)reval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );

/*
 * pcvar is returned as float no matter what. 
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

/*
 * Coerce trace to float.
 */
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rtrace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                                                   );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)eval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );
/*
 * pcvar is returned as float no matter what.
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)trace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }

/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 1;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcov_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *pcmsg;
  double *dx, *dpcmsg;
  float *rpcmsg;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  ng_size_t nrow, ncol, nobs, msta, total_size_x;
  int inrow, incol, inobs, imsta;
  int *neval, iopt = 0, jopt = 0;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *weval, *evecx;
  int   *iwork, *ifail;
  ng_size_t lwork, liwork, lifail;
  int ilwork, iliwork, ilifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
  double *trace, *eval;
  float *pcvar, *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol   = msta;
  nobs   = nrow = dsizes_x[ndims_x-1];
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (nobs > INT_MAX) || (lwork > INT_MAX) || (liwork > INT_MAX) || 
     (lifail > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow   = (int) nrow;
  incol   = (int) ncol;
  imsta   = (int) msta;
  inobs   = (int) nobs;
  ilwork  = (int) lwork;
  iliwork = (int) liwork;
  ilifail = (int) lifail;


/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if(dx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: Unable to allocate memory for coercing input to double precision");
    return(NhlFATAL);
  }
/*  
 * Go ahead and make a separate copy of pcmsg, even if it is already 
 * double, because we'll need to return that double later. We
 * don't want to return it as a pointer to the original double
 * value.
 */
  if(type_pcmsg != NCL_double) {
    dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);
  }
  else {
    dpcmsg  = (double*)calloc(1,sizeof(double));
    *dpcmsg = ((double*)pcmsg)[0];
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }

/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if(evec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  = (double *)calloc(*neval,sizeof(double));
  pcvar =  (float *)calloc(*neval,sizeof(float));
  if(trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  tmp_x  = (double *)calloc(total_size_x,sizeof(double));
  evecx  =  (double *)calloc(total_size_evec,sizeof(double));
  total_mem = 8*(lcssm+lwork+lifail+total_size_x+total_size_evec) +
              4*(liwork+lifail);
  if(  cssm == NULL ||  work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL || tmp_x == NULL || evecx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: Unable to allocate memory for work arrays. A total of %lld bytes need to be allocated",total_mem);
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(dncldrv,DNCLDRV)(dx,tmp_x,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,dpcmsg,evecx,cssm,&lcssm,
                           work,&ilwork,weval,iwork,&iliwork,ifail,&ilifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_pcmsg: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_pcmsg: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_pcmsg: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_pcmsg: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(tmp_x);
  NclFree(evecx);

/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)reval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );

/*
 * pcvar is returned as float no matter what. 
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

/*
 * Coerce trace to float.
 */
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
/*
 * Free double precision trace.
 */
    NclFree(trace);

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rtrace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                                                   );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)eval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );
/*
 * pcvar is returned as float no matter what.
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)trace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }

/*
 * Return pcmsg as an attribute "pcrit" of type float or double.
 */
  dsizes[0] = 1;
  if(type_pcmsg != NCL_double) {
    NclFree(dpcmsg);
    if(type_pcmsg != NCL_float) {
      rpcmsg = coerce_input_float(pcmsg,type_pcmsg,1,0,NULL,NULL);
    }
    else {
      rpcmsg  = (float*)calloc(1,sizeof(float));
      *rpcmsg = ((float*)pcmsg)[0];
    }
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)dpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  _NclAddAtt(
             att_id,
             "pcrit",
             att_md,
             NULL
             );
/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 2;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcor_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *pcmsg;
  double *dx, *dpcmsg;
  float *rpcmsg;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  ng_size_t nrow, ncol, nobs, msta, total_size_x;
  int inrow, incol, inobs, imsta;
  int *neval, iopt = 0, jopt = 1;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *weval, *evecx;
  int    *iwork, *ifail;
  ng_size_t lwork, liwork, lifail;
  int ilwork, iliwork, ilifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  int *eof_function;
  double *trace, *eval;
  float *pcvar, *rtrace, *reval;
/*
 * Output array variables
 */
  double *evec;
  float *revec;
  ng_size_t total_size_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);
/*
 * Get number of eigenvalues and eigen vectors to be computed.
 */
  neval = (int *)NclGetArgValue(
            1,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            DONT_CARE);

  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            DONT_CARE);
/*
 * The grid coming in must be at least 2-dimensional.
 */
  if( ndims_x < 2 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
/*
 * Check dimension sizes.
 */
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) msta *= dsizes_x[i];
  ncol   = msta;
  nobs   = nrow = dsizes_x[ndims_x-1];
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (nobs > INT_MAX) || (lwork > INT_MAX) || (liwork > INT_MAX) || 
     (lifail > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow   = (int) nrow;
  incol   = (int) ncol;
  imsta   = (int) msta;
  inobs   = (int) nobs;
  ilwork  = (int) lwork;
  iliwork = (int) liwork;
  ilifail = (int) lifail;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
  total_size_x = ncol * nrow;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  if(dx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: Unable to allocate memory for coercing input to double precision");
    return(NhlFATAL);
  }
/*  
 * Go ahead and make a separate copy of pcmsg, even if it is already 
 * double, because we'll need to return that double later. We
 * don't want to return it as a pointer to the original double
 * value.
 */
  if(type_pcmsg != NCL_double) {
    dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);
  }
  else {
    dpcmsg  = (double*)calloc(1,sizeof(double));
    *dpcmsg = ((double*)pcmsg)[0];
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec[0] = *neval;
  for( i = 0; i <= ndims_x-2; i++ ) dsizes_evec[i+1] = dsizes_x[i];

  total_size_evec = *neval * ncol;

  evec = (double *)calloc(total_size_evec,sizeof(double));
  if( evec == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Allocate memory for attributes.
 */
  trace = (double *)calloc(1,sizeof(double));
  eval  =  (double *)calloc(*neval,sizeof(double));
  pcvar = (float *)calloc(*neval,sizeof(float));
  if( trace == NULL || pcvar == NULL || eval == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: Unable to allocate memory for attribute arrays");
    return(NhlFATAL);
  }
/*
 * Create a few more work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  cssm   = (double *)calloc(lcssm,sizeof(double));
  work   = (double *)calloc(lwork,sizeof(double));
  weval  = (double *)calloc(lifail,sizeof(double));
  iwork  =    (int *)calloc(liwork,sizeof(int));
  ifail  =    (int *)calloc(lifail,sizeof(int));
  tmp_x  = (double *)calloc(total_size_x,sizeof(double));
  evecx  =  (double *)calloc(total_size_evec,sizeof(double));
  total_mem = 8*(lcssm+lwork+lifail+total_size_x+total_size_evec) +
              4*(liwork+lifail);
  if(  cssm == NULL ||  work == NULL || weval == NULL || iwork == NULL || 
      ifail == NULL || tmp_x == NULL || evecx == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: Unable to allocate memory for work arrays. A total of %lld bytes need to be allocated",total_mem);
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(dncldrv,DNCLDRV)(dx,tmp_x,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,dpcmsg,evecx,cssm,&lcssm,
                           work,&ilwork,weval,iwork,&iliwork,ifail,&ilifail,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_pcmsg: cssm contains one or more missing values.\n(One or more series contains all missing values.)" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_pcmsg: trace is equal to zero.\nAll data entries are missing or are equal to zero." );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_pcmsg: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_pcmsg: %d eigenvectors failed to converge.\nPoorly conditioned correlation matrix.",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  NclFree(work);
  NclFree(cssm);
  NclFree(weval);
  NclFree(iwork);
  NclFree(ifail);
  NclFree(tmp_x);
  NclFree(evecx);
/*
 * Return values. 
 */
  if(type_x != NCL_double) {
/*
 * Copy double values to float values.
 */
    revec = (float*)calloc(total_size_evec,sizeof(float));
    if( revec == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < total_size_evec; i++ ) revec[i] = (float)evec[i];

/*
 * Free up double precision array.
 */
    NclFree(evec);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec,
                              &missing_rx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
/*
 * Coerce eval to float.
 */
    reval = (float *)calloc(*neval,sizeof(float));
    for( i = 0; i < *neval; i++ ) reval[i] = (float)eval[i];
/*
 * Free double precision eval.
 */
    NclFree(eval);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)reval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );

/*
 * pcvar is returned as float no matter what. 
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

/*
 * Coerce trace to float.
 */
    rtrace = (float *)calloc(1,sizeof(float));
    *rtrace = (float)(*trace);
    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rtrace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                                                   );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }
  else {
/*
 *  Return doubles.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec,
                              &missing_dx,
                              ndims_x,
                              dsizes_evec,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );

    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)eval,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "eval",
               att_md,
               NULL
               );
/*
 * pcvar is returned as float no matter what.
 */
    dsizes[0] = *neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)pcvar,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "pcvar",
               att_md,
               NULL
               );

    dsizes[0] = 1;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)trace,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "trace",
               att_md,
               NULL
               );
  }
/*
 * Return pcmsg as an attribute "pcrit" of type float or double.
 */
  dsizes[0] = 1;
  if(type_pcmsg != NCL_double) {
    NclFree(dpcmsg);
    if(type_pcmsg != NCL_float) {
      rpcmsg = coerce_input_float(pcmsg,type_pcmsg,1,0,NULL,NULL);
    }
    else {
      rpcmsg  = (float*)calloc(1,sizeof(float));
      *rpcmsg = ((float*)pcmsg)[0];
    }
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)rpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
  }
  else {
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)dpcmsg,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
  }
  _NclAddAtt(
             att_id,
             "pcrit",
             att_md,
             NULL
             );
/*
 * eof_function is returned to indicate which function was used.
 */
  eof_function = (int *)calloc(1,sizeof(int));
  *eof_function = 3;
  dsizes[0] = 1;
  att_md = _NclCreateVal(
                         NULL,
                         NULL,
                         Ncl_MultiDValData,
                         0,
                         (void*)eof_function,
                         NULL,
                         1,
                         dsizes,
                         TEMPORARY,
                         NULL,
                         (NclObjClass)nclTypeintClass
                         );
  _NclAddAtt(
             att_id,
             "eof_function",
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


NhlErrorTypes eofcov_ts_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *devec;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int inrow, incol, inobs, imsta;
  ng_size_t nrow, ncol, nobs, msta, neval, ntime, total_size_x, total_size_evec;
  int ineval, iflag = 0, jopt = 0;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  ng_size_t dsizes_evec_ts[2];
/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;


/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];
 
  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x/evec to double if necessary.
 */
  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,&missing_devec);
  if(dx == NULL || devec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variables.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  evtsav  = (double *)calloc(neval,sizeof(double));
  if( evec_ts == NULL || evtsav == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the deofts7 Fortran 77 routine.
 */
  NGCALLF(deofts7,DEOFTS7)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,&ineval,devec,
                           &jopt,&iflag,wx,wrk,evec_ts,evtsav,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) { 
       NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)devec != evec) NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    revtsav  = (float *)calloc(neval,sizeof(float));
    if( revec_ts == NULL || revtsav == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
    for( i = 0; i < neval; i++ )       revtsav[i]  = (float)evtsav[i];
/*
 * Free up double precision arrays.
 */
    NclFree(evec_ts);
    NclFree(evtsav);
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec_ts,
                              &missing_rx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)revtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );

  }
  else {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec_ts,
                              &missing_dx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)evtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );
  }
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


NhlErrorTypes eofcor_ts_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *devec;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int inrow, incol, inobs, imsta;
  ng_size_t nrow, ncol, nobs, msta, neval, ntime, total_size_x, total_size_evec;
  int ineval, iflag = 0, jopt = 1;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  ng_size_t dsizes_evec_ts[2];

/*
 * Attribute variables
 */
  int att_id;
  ng_size_t dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           2,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x/evec to double if necessary.
 */
  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,&missing_devec);
  if(dx == NULL || devec == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variables.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  evtsav  = (double *)calloc(neval,sizeof(double));
  if( evec_ts == NULL || evtsav == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the deofts7 Fortran 77 routine.
 */
  NGCALLF(deofts7,DEOFTS7)(dx,&inrow,&incol,&inobs,&imsta,
                           &missing_dx.doubleval,&ineval,devec,
                           &jopt,&iflag,wx,wrk,evec_ts,evtsav,&ier);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx != x) NclFree(dx);
  if((void*)devec != evec) NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    revtsav  = (float *)calloc(neval,sizeof(float));
    if( revec_ts == NULL || revtsav == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: Unable to allocate memory for output arrays");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
    for( i = 0; i < neval; i++ )       revtsav[i]  = (float)evtsav[i];
/*
 * Free up double precision arrays.
 */
    NclFree(evec_ts);
    NclFree(evtsav);

/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)revec_ts,
                              &missing_rx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypefloatClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)revtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypefloatClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );

  }
  else {
/*
 * Set up return value.
 */
    return_md = _NclCreateVal(
                              NULL,
                              NULL,
                              Ncl_MultiDValData,
                              0,
                              (void*)evec_ts,
                              &missing_dx,
                              2,
                              dsizes_evec_ts,
                              TEMPORARY,
                              NULL,
                              (NclObjClass)nclTypedoubleClass
                              );
/*
 * Set up attributes to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);

/*
 * Attribute "ts_mean".
 */
    dsizes[0] = neval;
    att_md = _NclCreateVal(
                           NULL,
                           NULL,
                           Ncl_MultiDValData,
                           0,
                           (void*)evtsav,
                           NULL,
                           1,
                           dsizes,
                           TEMPORARY,
                           NULL,
                           (NclObjClass)nclTypedoubleClass
                           );
    _NclAddAtt(
               att_id,
               "ts_mean",
               att_md,
               NULL
               );
  }
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


NhlErrorTypes eofcov_ts_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec, *pcmsg;
  double *dx, *devec, *dpcmsg;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec, type_pcmsg;
  ng_size_t nrow, ncol, nobs, msta, ntime, neval, total_size_x, total_size_evec;
  int inrow, incol, inobs, imsta, ineval;
  int iflag = 0, jopt = 0;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  ng_size_t dsizes_evec_ts[2];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           3,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            DONT_CARE);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x, evec, pcmsg to double if necessary.
 */
  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;
  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,&missing_devec);
  dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);

  if(dx == NULL || devec == NULL || dpcmsg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: Unable to allocate memory for coercing input to double precision");
    return(NhlFATAL);
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  if( evec_ts == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(deoftsca,DEOFTSCA)(dx,&inrow,&incol,&inobs,&imsta,
                             &missing_dx.doubleval,&ineval,devec,
                             &jopt,&iflag,evec_ts,dpcmsg,&ier,wx,wrk);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts_pcmsg: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts_pcmsg: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts_pcmsg: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcov_ts_pcmsg: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx     != x)     NclFree(dx);
  if((void*)dpcmsg != pcmsg) NclFree(dpcmsg);
  if((void*)devec  != evec)  NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    if( revec_ts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free up double precision array.
 */
    NclFree(evec_ts);

/*
 * Return float values. 
 */
    return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes eofcor_ts_pcmsg_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec, *pcmsg;
  double *dx, *devec, *dpcmsg;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  int has_missing_x;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec, type_pcmsg;
  ng_size_t nrow, ncol, nobs, msta, neval, ntime, total_size_x, total_size_evec;
  int inrow, incol, inobs, imsta, ineval;
  int iflag = 0, jopt = 1;
  ng_size_t i;
  int ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  ng_size_t lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  ng_size_t dsizes_evec_ts[2];

/*
 * Retrieve parameters
 */
  x = (void*)NclGetArgValue(
           0,
           3,
           &ndims_x, 
           dsizes_x,
           &missing_x,
           &has_missing_x,
           &type_x,
           DONT_CARE);

  evec = (void*)NclGetArgValue(
           1,
           3,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
  pcmsg = (void *)NclGetArgValue(
            2,
            3, 
            NULL,
            NULL,
            NULL,
            NULL,
            &type_pcmsg,
            DONT_CARE);
/*
 * Check the input grids.  They both must be at least two dimensional and
 * have the same number of dimensions.  All but the last dimension of the
 * first input array must be the same as all the but first dimension of
 * the second input array.
 */
  if( ndims_x < 2 || ndims_x != ndims_evec ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: The input arrays must be at least 2-dimensional and have the same number of dimensions");
    return(NhlFATAL);
  }
  msta = 1;
  for( i = 0; i <= ndims_x-2; i++ ) {
    if( dsizes_x[i] != dsizes_evec[i+1] ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }

  if((nrow > INT_MAX) || (ncol > INT_MAX) || (msta > INT_MAX) || 
     (neval > INT_MAX) || (nobs > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inrow  = (int) nrow;
  incol  = (int) ncol;
  imsta  = (int) msta;
  inobs  = (int) nobs;
  ineval = (int) neval;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x, evec, pcmsg to double if necessary.
 */
  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  dx = coerce_input_double(x,type_x,total_size_x,has_missing_x,&missing_x,
                           &missing_dx);
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,&missing_devec);
  dpcmsg = coerce_input_double(pcmsg,type_pcmsg,1,0,NULL,NULL);

  if(dx == NULL || devec == NULL || dpcmsg == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: Unable to allocate memory for coercing input to double precision");
    return(NhlFATAL);
  }
/*
 * Check dpcmsg
 */
  if(*dpcmsg < 0. || *dpcmsg > 100.) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: pcmsg must be between 0 and 100 inclusive");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_evec_ts[0] = neval;
  dsizes_evec_ts[1] = ntime;
  evec_ts = (double *)calloc(ntime*neval,sizeof(double));
  if( evec_ts == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
/*
 * Create a couple of work arrays.  This is necessary to avoid having
 * these arrays created dynamically in the Fortran file (which makes
 * it Fortran 90, and unportable to some systems. 
 */
  lwrk = nobs;
  lwx  = nrow*ncol;
  wrk  = (double *)calloc(lwrk,sizeof(double));
  wx   = (double *)calloc(lwx,sizeof(double));
  if( wrk == NULL || wx == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: Unable to allocate memory for work arrays");
    return(NhlFATAL);
  }
/*
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(deoftsca,DEOFTSCA)(dx,&inrow,&incol,&inobs,&imsta,
                             &missing_dx.doubleval,&ineval,devec,
                             &jopt,&iflag,evec_ts,dpcmsg,&ier,wx,wrk);
/*
 * Check various possible error messages.
 */
  if (ier != 0) {
    if (ier == -1) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts_pcmsg: cssm contains one or more missing values" );
    }
    else if (ier == -88) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts_pcmsg: trace is equal to zero" );
    }
    else if (ier < 0) {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts_pcmsg: The %d-th argument had an illegal value", abs(ier) );
    }
    else {
      NhlPError(NhlWARNING,NhlEUNKNOWN,"eofcor_ts_pcmsg: %d eigenvectors failed to converge",ier);
    }
  }
/*
 * Free unneeded memory.
 */
  if((void*)dx     != x)     NclFree(dx);
  if((void*)dpcmsg != pcmsg) NclFree(dpcmsg);
  if((void*)devec  != evec)  NclFree(devec);
  NclFree(wx);
  NclFree(wrk);
/*
 * Return values. 
 */
  if(type_x != NCL_double && type_evec != NCL_double) {
/*
 * Neither input array is double, so return float values.
 *
 * First copy double values to float values.
 */
    revec_ts = (float *)calloc(ntime*neval,sizeof(float));
    if( revec_ts == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: Unable to allocate memory for output array");
      return(NhlFATAL);
    }
    for( i = 0; i < ntime*neval; i++ ) revec_ts[i] = (float)evec_ts[i];
/*
 * Free up double precision array.
 */
    NclFree(evec_ts);

/*
 * Return float values. 
 */
    return(NclReturnValue((void*)revec_ts,2,dsizes_evec_ts,&missing_rx,
                          NCL_float,0));
  }
  else {
/*
 * Return double values. 
 */
    return(NclReturnValue((void*)evec_ts,2,dsizes_evec_ts,&missing_dx,
                          NCL_double,0));
  }
}


NhlErrorTypes eof2data_W( void )
{
/*
 * Input array variables
 */
  void *evec, *evects;
  double *devec, *devects;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  ng_size_t dsizes_evects[2];
  NclScalar missing_evec, missing_devec, missing_revec;
  NclBasicDataTypes type_evec, type_evects;
  ng_size_t neval, npts, ntim, total_size_evec, total_size_evects;
  int ineval, inpts, intim;
  ng_size_t i;
  int ret;
/*
 * Output array variables
 */
  void *x;
  double *dx;
  ng_size_t *dsizes_x;
  ng_size_t total_size_x;
  NclBasicDataTypes type_x;

/*
 * Retrieve parameters
 */
  evec = (void*)NclGetArgValue(
           0,
           2,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
  evects = (void*)NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_evects,
           NULL,
           NULL,
           &type_evects,
           DONT_CARE);
/*
 * Check the input grids. The first one can be any dimension, but it must
 * be at least 2 dimensions.  The first dimension of both input arrays
 * must be the same (neval).
 */
  if(ndims_evec < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data: The first input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  if( dsizes_evec[0] != dsizes_evects[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data: The leftmost dimension of both input arrays must be the same");
    return(NhlFATAL);
  }

  neval = dsizes_evec[0];
  ntim  = dsizes_evects[1];

  npts  = 1;
  for( i = 1; i < ndims_evec; i++ ) npts *= dsizes_evec[i];

  if((neval > INT_MAX) || (npts > INT_MAX) || (npts > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  ineval = (int) neval;
  inpts  = (int) npts;
  intim  = (int) ntim;

  total_size_evec   = neval * npts;
  total_size_evects = neval * ntim;
  total_size_x      = npts  * ntim;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,&missing_devec,
                 &missing_revec);
/*
 * Coerce evec/evects to double if necessary.
 */
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,
                              &missing_devec);
  devects = coerce_input_double(evects,type_evects,total_size_evects,
                                0,NULL,NULL);
  if(devec == NULL || devects == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_x = (ng_size_t *)calloc(ndims_evec, sizeof(ng_size_t));
  
  if(type_evec == NCL_double || type_evects == NCL_double) {
    type_x = NCL_double;
    x      = (double *)calloc(total_size_x,sizeof(double));
    dx     = (double*)x;
  }
  else {
    type_x = NCL_float;
    x      = (float *)calloc(total_size_x,sizeof(float));
    dx     = (double *)calloc(total_size_x,sizeof(double));
  }
  if(x == NULL || dx == NULL || dsizes_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  dsizes_x[ndims_evec-1] = ntim;
  for(i=0; i <= ndims_evec-2; i++) dsizes_x[i] = dsizes_evec[i+1];

/*
 * Call the Fortran 77 version of 'deof2data' with the full argument list.
 */
  NGCALLF(deof2data,DEOF2DATA)(&ineval,&inpts,&intim,devec,devects,dx,
                               &missing_devec.doubleval);
/*
 * Free unneeded memory.
 */
  if((void*)devec   != evec)   NclFree(devec);
  if((void*)devects != evects) NclFree(devects);
/*
 * Return values. 
 */
  if(type_x == NCL_float) {
/*
 * Coerce double values to float and free up double precision array.
 */
    coerce_output_float_only(x,dx,total_size_x,0);

    if(has_missing_evec) {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,&missing_revec,type_x,0);
    }
    else {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,NULL,type_x,0);
    }
    NclFree(dx);
  }
  else {
/*
 * Return double values. 
 */
    if(has_missing_evec) {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,&missing_devec,type_x,0);
    }
    else {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,NULL,type_x,0);
    }
  }
  NclFree(dsizes_x);
  return(ret);
}


/*
 * This wrapper is basically identical to eof2data_W in how it is coded,
 * but it potentially reorders the data at the very end to put the
 * "time" dimension wherever the user requests via the "dim" argument.
 *
 * eof2data_W assumes time is always the rightmost dimension.
 */
NhlErrorTypes eof2data_n_W( void )
{
/*
 * Input array variables
 */
  void *evec, *evects;
  double *devec, *devects;
  int *dim;
  int ndims_evec;
  ng_size_t dsizes_evec[NCL_MAX_DIMENSIONS];
  int has_missing_evec;
  ng_size_t dsizes_evects[2];
  NclScalar missing_evec, missing_devec, missing_revec;
  NclBasicDataTypes type_evec, type_evects;
  ng_size_t neval, npts, ntim, total_size_evec, total_size_evects;
  int ineval, inpts, intim;
  ng_size_t i;
  int ret;
/*
 * Output array variables
 */
  void *x;
  double *dx;
  ng_size_t *dsizes_x;
  ng_size_t total_size_x;
  NclBasicDataTypes type_x;
 /*
  * Various
  */
  ng_size_t nr, nl, nm, counter, ireordered;
  ng_size_t size_leftmost, size_middle, size_rightmost, size_middle_rightmost;
  ng_size_t left_loc, mid_loc;
  logical time_is_rightmost;

/*
 * Retrieve parameters
 */
  evec = (void*)NclGetArgValue(
           0,
           3,
           &ndims_evec, 
           dsizes_evec,
           &missing_evec,
           &has_missing_evec,
           &type_evec,
           DONT_CARE);
  evects = (void*)NclGetArgValue(
           1,
           3,
           NULL,
           dsizes_evects,
           NULL,
           NULL,
           &type_evects,
           DONT_CARE);

 /*
  * Retrieve the dimension index for the "time" dimension.
  */ 
  dim = (int*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Check the "dim" argument. It must not be greater than the number of
 * dimensions of evec.
 */
  if(*dim < 0 || *dim > ndims_evec) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: The 'dim' argument is invalid.");
    return(NhlFATAL);
  }
  if(*dim == (ndims_evec-1)) {
    time_is_rightmost = True;
  }
  else {
    time_is_rightmost = False;
  }

/*
 * Check the input grids. The first one can be any dimension, but it must
 * be at least 2 dimensions.  The first dimension of both input arrays
 * must be the same (neval).
 */
  if(ndims_evec < 2) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: The first input array must be at least 2-dimensional");
    return(NhlFATAL);
  }

  if( dsizes_evec[0] != dsizes_evects[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: The leftmost dimension of both input arrays must be the same");
    return(NhlFATAL);
  }

  neval = dsizes_evec[0];
  ntim  = dsizes_evects[1];

  npts  = 1;
  for( i = 1; i < ndims_evec; i++ ) npts *= dsizes_evec[i];

  if((neval > INT_MAX) || (npts > INT_MAX) || (npts > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: one or more dimension sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  ineval = (int) neval;
  inpts  = (int) npts;
  intim  = (int) ntim;

  total_size_evec   = neval * npts;
  total_size_evects = neval * ntim;
  total_size_x      = npts  * ntim;

/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_evec,has_missing_evec,&missing_evec,&missing_devec,
                 &missing_revec);
/*
 * Coerce evec/evects to double if necessary.
 */
  devec = coerce_input_double(evec,type_evec,total_size_evec,
                              has_missing_evec,&missing_evec,
                              &missing_devec);
  devects = coerce_input_double(evects,type_evects,total_size_evects,
                                0,NULL,NULL);
  if(devec == NULL || devects == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: Unable to allocate memory for coercing input arrays to double precision");
    return(NhlFATAL);
  }
/*
 * Allocate memory for return variable.
 */
  dsizes_x = (ng_size_t *)calloc(ndims_evec, sizeof(ng_size_t));
  
  if(type_evec == NCL_double || type_evects == NCL_double) {
    type_x = NCL_double;
    x      = (double *)calloc(total_size_x,sizeof(double));
    if(time_is_rightmost) {
      dx   = (double*)x;
    }
    else {
      dx   = (double *)calloc(total_size_x,sizeof(double));
    }
  }
  else {
    type_x = NCL_float;
    x      = (float *)calloc(total_size_x,sizeof(float));
    dx     = (double *)calloc(total_size_x,sizeof(double));
  }
  if(x == NULL || dx == NULL || dsizes_x == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eof2data_n: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call the Fortran 77 version of 'deof2data' with the full argument list.
 */
  NGCALLF(deof2data,DEOF2DATA)(&ineval,&inpts,&intim,devec,devects,dx,
                               &missing_devec.doubleval);
/*
 * Free unneeded memory.
 */
  if((void*)devec   != evec)   NclFree(devec);
  if((void*)devects != evects) NclFree(devects);

/*
 * Here's where we need to reorder the data if the time
 * dimension is not the rightmost dimension (the "else"
 * part of this "if" statement).
 *
 * First create a vector containing the reordered indices, based
 * on the 'dim' argument. Then use this to reorder the "dx" 
 * array such that the time dimension is the "dim"-th dimension.
 */
  if(!time_is_rightmost) {
    size_rightmost = size_leftmost = 1;
    for( i = 0; i < *dim; i++ ) {
      size_leftmost *= dsizes_evec[i+1];
      dsizes_x[i] = dsizes_evec[i+1];
    }
    for( i = *dim+1; i < ndims_evec; i++ ) {
      size_rightmost *= dsizes_evec[i];
      dsizes_x[i] = dsizes_evec[i];
    }
    dsizes_x[*dim] = size_middle = ntim;
    size_middle_rightmost = size_rightmost * size_middle;

    counter = 0;
    for(nl = 0; nl < size_leftmost; nl++) {
      left_loc = nl * size_middle_rightmost;
      for(nr = 0; nr < size_rightmost; nr++) {
        for(nm = 0; nm < size_middle; nm++) {
          mid_loc = nm * size_rightmost;
          ireordered = left_loc + mid_loc + nr;
          if(type_x == NCL_float) {
            ((float*)x)[ireordered] = (float)dx[counter];
          }
          else {
            ((double*)x)[ireordered] = dx[counter];
          }
          counter++;
        }
      }
    }
  }
  else {
    dsizes_x[ndims_evec-1] = ntim;
    for(i=0; i <= ndims_evec-2; i++) dsizes_x[i] = dsizes_evec[i+1];

/*
 * Return values. 
 */
    if(type_x == NCL_float) {
/*
 * Coerce double values to float and free up double precision array.
 */
      coerce_output_float_only(x,dx,total_size_x,0);
    }
  }
  if(type_x == NCL_float) {
    if(has_missing_evec) {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,&missing_revec,type_x,0);
    }
    else {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,NULL,type_x,0);
    }
  }
  else {
/*
 * Return double values. 
 */
    if(has_missing_evec) {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,&missing_devec,type_x,0);
    }
    else {
      ret = NclReturnValue(x,ndims_evec,dsizes_x,NULL,type_x,0);
    }
  }
  if(type_x != NCL_double || !time_is_rightmost) {
    NclFree(dx);
  }
  NclFree(dsizes_x);
  return(ret);
}

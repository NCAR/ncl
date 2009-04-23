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


NhlErrorTypes eof_W( void )
{
/*
 * Input array variables
 */
  void *x;
  double *dx;
  logical *opt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, mcsta, nc, nc2, nr, kntx, total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  double *pcrit;
  float *rpcrit, scale_factor;
  NclBasicDataTypes type_pcrit;
  int iopt = 0, jopt = 0, i, j, l1, l2, ier = 0;
  logical tr_setbyuser = False, anomalies = False, debug = False;
  logical use_new_transpose = False, use_old_transpose = False;
  logical return_eval = True, return_trace = False, return_pcrit = False;
/*
 * Work array variables.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int icovcor, lwork, liwork, lifail, lweval, lcssm;
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
  int dsizes[1];
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
  char *cmatrix, *cmethod;
  NclQuark *matrix, *method;
  double *trace, *eval, *eval2, *pcvar, *prncmp;
  float *rpcvar, *rtrace, *reval, *reval2;
/*
 * Output array variables
 */
  double *evec, *wevec, *xdatat;
  float *revec;
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];

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
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc: The input array must be at least 2-dimensional");
    return(NhlFATAL);
  }
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
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&nrow,&missing_dx.doubleval,
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
/*
 * Depending on the size of the rightmost 2D arrays being processed, and/or
 * the value of the transpose or oldtranspose attributes, we call one of
 * three different Fortran routines. These routines basically behave the
 * same, except two of them operate on a transposed version of the 2d array.
 */
  if(debug) {
    printf("eofunc: msta = %d mcsta = %d nobs = %d\n", msta, mcsta, nobs);
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
    llcssm = mcsta*(mcsta+1)/2;
    lwork  = 8*mcsta;
    liwork = 5*mcsta;
    lifail = mcsta;
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
    NGCALLF(deof11,DEOF11)(xdatat,&mcsta,&nrow,neval,&icovcor,
                           &missing_dx.doubleval,eval,wevec,pcvar,prncmp);
  }
  else if(use_old_transpose) {
    NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&nrow,&ncol,&nobs,&mcsta,
                             &missing_dx.doubleval,neval,eval,evec,
                             pcvar,trace,xdvar,xave,&jopt,&ier);
  }
  else {
    NGCALLF(ddrveof,DDRVEOF)(dx_strip,&nrow,&ncol,&nobs,&mcsta,
                             &missing_dx.doubleval,neval,eval,wevec,rpcvar,
                             trace,&iopt,&jopt,cssm,&llcssm,work,&lwork,
                             weval,iwork,&liwork,ifail,&lifail,&ier);
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
    if(!use_new_transpose && return_trace) {
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

    if(!use_new_transpose && return_trace) {
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
 * Return output grid and attributes to NCL.
 */
  return_data.kind = NclStk_VAR;
  return_data.u.data_var = tmp_var;
  _NclPlaceReturn(return_data);

  return(NhlNOERROR);
}


NhlErrorTypes eof_ts_W( void )
{
/*
 * Input array variables
 */
  void *x, *evec;
  double *dx, *devec;
  logical *opt;
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, iflag = 0, jopt = 0, i, ier = 0;

/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  int dsizes_evec_ts[2];
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
  int dsizes[1];
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
      NhlPError(NhlFATAL,NhlEUNKNOWN,"eofunc_ts: All but the last dimension of the first input array must be the same as all but the first dimension of the second input array");
      return(NhlFATAL);
    }
    msta *= dsizes_x[i];
  }
  ncol = msta;
  nobs = nrow = ntime = dsizes_x[ndims_x-1];
  neval = dsizes_evec[0];

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
  NGCALLF(deofts7,DEOFTS7)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           &neval,devec,&jopt,&iflag,wx,wrk,evec_ts,evtsav,
                           &ier);

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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, mcsta, nc, nc2, nr, kntx, total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  double *pcrit;
  float *rpcrit, *rpcvar;
  NclBasicDataTypes type_pcrit;
  int iopt = 0, jopt = 0, i, j, l1, l2, ier = 0, icovcor;
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
  int dsizes[1];
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
  double *evec, *wevec, *xdatat;
  float *revec;
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];

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

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
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
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&nrow,&missing_dx.doubleval,
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

  if(debug) {
    printf("eofcov_tr: msta = %d mcsta = %d nobs = %d\n", msta, mcsta, nobs);
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
  NGCALLF(deof11,DEOF11)(xdatat,&mcsta,&nrow,neval,&icovcor,
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, mcsta, nc, nr, kntx, total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *pcrit, *xdatat;
  float *rpcrit;
  NclBasicDataTypes type_pcrit;
  int iopt = 0, jopt = 0, i, ier = 0;
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
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];

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

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
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
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&nrow,&missing_dx.doubleval,
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

/*
 * We have to allocate this array separately, because it depends
 * on the value of "mcsta".
 */
  xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
  if(xdatat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_tr_old: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

  NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&nrow,&ncol,&nobs,&mcsta,
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, mcsta, nc, nr, kntx, total_size_x;
  int *neval, ne;
/*
 * Various.
 */
  double *dx_strip, *xave, *xdvar, *xvar, con, pcx, xsd;
  double *pcrit, *xdatat;
  float *rpcrit;
  NclBasicDataTypes type_pcrit;
  int iopt = 0, jopt = 1, i, ier = 0;
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
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];

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

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
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
    NGCALLF(dstat2,DSTAT2)(&dx[nrow*nc],&nrow,&missing_dx.doubleval,
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

/*
 * We have to allocate this array separately, because it depends
 * on the value of "mcsta".
 */
  xdatat = (double *)calloc(nrow*mcsta,sizeof(double));
  if(xdatat == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_tr: Unable to allocate memory for stripping the data");
    return(NhlFATAL);
  }

  NGCALLF(xrveoft,XRVEOFT)(dx_strip,xdatat,&nrow,&ncol,&nobs,&mcsta,
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
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
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
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
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
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
  NGCALLF(ddrveof,DDRVEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           neval,eval,evec,pcvar,trace,&iopt,&jopt,
                           cssm,&lcssm,work,&lwork,weval,iwork,&liwork,
                           ifail,&lifail,&ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *cssm, *work, *weval;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
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
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
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
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
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
  NGCALLF(ddrveof,DDRVEOF)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           neval,eval,evec,pcvar,trace,&iopt,&jopt,
                           cssm,&lcssm,work,&lwork,weval,iwork,&liwork,
                           ifail,&lifail,&ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *weval, *evecx;
  int   *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
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
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
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
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
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
  NGCALLF(dncldrv,DNCLDRV)(dx,tmp_x,&nrow,&ncol,&nobs,&msta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,dpcmsg,evecx,cssm,&lcssm,
                           work,&lwork,weval,iwork,&liwork,ifail,&lifail,&ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  NclScalar missing_x, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x;
  int *neval, iopt = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *tmp_x, *cssm, *work, *weval, *evecx;
  int    *iwork, *ifail;
  int lwork, liwork, lifail;
  long long int lcssm, total_mem;
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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
  int total_size_evec, dsizes_evec[NCL_MAX_DIMENSIONS];
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
  ncol = msta;
  nobs = nrow = dsizes_x[ndims_x-1];

  total_size_x = ncol * nrow;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
/*
 * Coerce x to double if necessary.
 */
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
  lcssm  = msta*(msta+1)/2;
  lwork  = 8*msta;
  liwork = 5*msta;
  lifail = msta;
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
  NGCALLF(dncldrv,DNCLDRV)(dx,tmp_x,&nrow,&ncol,&nobs,&msta,
                           &missing_dx.doubleval,neval,eval,evec,pcvar,
                           trace,&iopt,&jopt,dpcmsg,evecx,cssm,&lcssm,
                           work,&lwork,weval,iwork,&liwork,ifail,&lifail,&ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, iflag = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  int dsizes_evec_ts[2];
/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts: The dimensions of the input array must both be at least 1");
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
  NGCALLF(deofts7,DEOFTS7)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           &neval,devec,&jopt,&iflag,wx,wrk,evec_ts,evtsav,
                           &ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, iflag = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts, *evtsav;
  float *revec_ts, *revtsav;      
  int dsizes_evec_ts[2];

/*
 * Attribute variables
 */
  int att_id;
  int dsizes[1];
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

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts: The dimensions of the input array must both be at least 1");
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
 * Call the Fortran 77 version of 'drveof' with the full argument list.
 */
  NGCALLF(deofts7,DEOFTS7)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                           &neval,devec,&jopt,&iflag,wx,wrk,evec_ts,evtsav,
                           &ier);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, iflag = 0, jopt = 0, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  int dsizes_evec_ts[2];

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

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcov_ts_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x, evec, pcmsg to double if necessary.
 */
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
  NGCALLF(deoftsca,DEOFTSCA)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             &neval,devec,&jopt,&iflag,evec_ts,dpcmsg,&ier,wx,wrk);
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
  int ndims_x, dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  NclScalar missing_x, missing_evec, missing_devec, missing_rx, missing_dx;
  NclBasicDataTypes type_x, type_evec, type_pcmsg;
  int nrow, ncol, nobs, msta, total_size_x, total_size_evec;
  int neval, ntime, iflag = 0, jopt = 1, i, ier = 0;
/*
 * Work array variables.
 */
  double *wrk, *wx;
  int lwrk, lwx;
/*
 * Output array variables
 */
  double *evec_ts;
  float *revec_ts;      
  int dsizes_evec_ts[2];

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

  total_size_x    = ncol * nrow;
  total_size_evec = msta * neval;

  if( msta < 1 || nobs < 1 ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"eofcor_ts_pcmsg: The dimensions of the input array must both be at least 1");
    return(NhlFATAL);
  }
/*
 * Coerce missing values, if any.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_evec,has_missing_evec,&missing_evec,
                 &missing_devec,NULL);
/*
 * Coerce x, evec, pcmsg to double if necessary.
 */
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
  NGCALLF(deoftsca,DEOFTSCA)(dx,&nrow,&ncol,&nobs,&msta,&missing_dx.doubleval,
                             &neval,devec,&jopt,&iflag,evec_ts,dpcmsg,&ier,wx,wrk);
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
  int ndims_evec, dsizes_evec[NCL_MAX_DIMENSIONS], has_missing_evec;
  int dsizes_evects[2], has_missing_evects;
  NclScalar missing_evec, missing_devec, missing_revec;
  NclBasicDataTypes type_evec, type_evects;
  int nrow, ncol, nobs, msta, total_size_evec, total_size_evects;
  int neval, npts, ntim, i, ret;
/*
 * Output array variables
 */
  void *x;
  double *dx;
  float *rx;      
  int *dsizes_x, total_size_x;
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
  dsizes_x = (int*)calloc(ndims_evec, sizeof(int));
  
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
  NGCALLF(deof2data,DEOF2DATA)(&neval,&npts,&ntim,devec,devects,dx,
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
    NclFree(dx);

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
  NclFree(dsizes_x);
  return(ret);
}

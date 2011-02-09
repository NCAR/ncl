#include <stdio.h>
#include <math.h>
#include "wrapper.h"

extern void NGCALLF(dskewty,DSKEWTY)(double*,int*,double*);
extern void NGCALLF(dskewtx,DSKEWTX)(double*,double*,int*,double*);
extern double NGCALLF(dtmrskewt,DTMRSKEWT)(double*,double*);
extern double NGCALLF(dtdaskewt,DTDASKEWT)(double*,double*);
extern double NGCALLF(dsatlftskewt,DSATLFTSKEWT)(double*,double*);
extern void NGCALLF(dptlclskewt,DPTLCLSKEWT)(double*,double*,double*,
                                             double*,double*);
extern double NGCALLF(dshowalskewt,DSHOWALSKEWT)(double*,double*,double*,int*);
extern double NGCALLF(dpwskewt,DPWSKEWT)(double*,double*,int*);
extern double NGCALLF(dcapethermo,DCAPETHERMO)(double*,double*,int*,double*,
                                               int*,double*,double*,int*,int*,
                                               int*);

NhlErrorTypes y_skewt_W( void )
{
/*
 * Input array variables
 */
  void *pres;
  ng_size_t np;
  int inp;
  double *tmp_pres;
  ng_size_t dsizes_pres[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pres;
/*
 * Output array variables
 */
  void *yskewt;
  double *tmp_yskewt;
/*
 * Retrieve arguments.
 */
  pres = (void*)NclGetArgValue(
          0,
          1,
          NULL,
          dsizes_pres,
          NULL,
          NULL,
          &type_pres,
          DONT_CARE);

  np = dsizes_pres[0];

/*
 * Test dimension sizes.
 */
  if(np > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: the length of p is greater than INT_MAX");
    return(NhlFATAL);
  }
  inp = (int) np;

/*
 * Create a temporary array to hold subarray of pres and yskewt.
 */
  tmp_yskewt = (double*)calloc(np,sizeof(double));
  if(tmp_yskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }
  tmp_pres = coerce_input_double(pres,type_pres,np,0,NULL,NULL);
  if(tmp_pres == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to coerce 'pres' to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_pres != NCL_double) {
    yskewt = (void*)calloc(np,sizeof(float));
  }
  else {
    yskewt = (void*)calloc(np,sizeof(double));
  }
  if( yskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"y_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  NGCALLF(dskewty,DSKEWTY)(tmp_pres,&inp,tmp_yskewt);

  coerce_output_float_or_double(yskewt,tmp_yskewt,type_pres,np,0);

/*
 * Free memory.
 */
  if(type_pres != NCL_double) NclFree(tmp_pres);
  NclFree(tmp_yskewt);

  return(NclReturnValue(yskewt,1,dsizes_pres,NULL,type_pres,0));
}

NhlErrorTypes x_skewt_W( void )
{
/*
 * Input array variables
 */
  void *temp, *y;
  ng_size_t nty;
  int inty;
  double *tmp_temp, *tmp_y;
  ng_size_t dsizes_temp[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_temp, type_y;
/*
 * Output array variables
 */
  void *xskewt;
  double *tmp_xskewt;
  NclBasicDataTypes type_xskewt;
/*
 * Retrieve arguments.
 */
  temp = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_temp,
          NULL,
          NULL,
          &type_temp,
          DONT_CARE);
  y = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          dsizes_y,
          NULL,
          NULL,
          &type_y,
          DONT_CARE);
/*
 * temp and y must be the same size.
 */
  if(dsizes_temp[0] != dsizes_y[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: The input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }

  nty = dsizes_temp[0];

/*
 * Test dimension sizes.
 */
  if(nty > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: the length of temp is greater than INT_MAX");
    return(NhlFATAL);
  }
  inty = (int) nty;

/*
 * Create a temporary array to hold subarray of temp, y, and xskewt.
 */
  tmp_xskewt = (double*)calloc(nty,sizeof(double));
  if(tmp_xskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  tmp_temp = coerce_input_double(temp,type_temp,nty,0,NULL,NULL);
  tmp_y    = coerce_input_double(y,type_y,nty,0,NULL,NULL);
  if(tmp_temp == NULL || tmp_y == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_temp == NCL_double || type_y == NCL_double) {
    type_xskewt = NCL_double;
    xskewt = (void*)calloc(nty,sizeof(double));
  }
  else {
    type_xskewt = NCL_float;
    xskewt = (void*)calloc(nty,sizeof(float));
  }
  if( xskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"x_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  NGCALLF(dskewtx,DSKEWTX)(tmp_temp,tmp_y,&inty,tmp_xskewt);

  coerce_output_float_or_double(xskewt,tmp_xskewt,type_xskewt,nty,0);

/*
 * Free memory.
 */
  if(type_temp != NCL_double) NclFree(tmp_temp);
  if(type_y    != NCL_double) NclFree(tmp_y);
  NclFree(tmp_xskewt);

  return(NclReturnValue(xskewt,1,dsizes_temp,NULL,type_xskewt,0));
}

NhlErrorTypes tmr_skewt_W( void )
{
/*
 * Input array variables
 */
  void *w, *p;
  double *tmp_w, *tmp_p;
  ng_size_t dsizes[1];
  NclBasicDataTypes type_w, type_p;
/*
 * Output array variables
 */
  void *tmrskewt;
  double *tmp_tmrskewt;
  NclBasicDataTypes type_tmrskewt;
/*
 * Retrieve arguments.
 */
  w = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_w,
          DONT_CARE);
  p = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
/*
 * Create temp arrays to hold subarray of w, y, and tmrskewt.
 */
  tmp_tmrskewt = (double*)calloc(1,sizeof(double));
  if(tmp_tmrskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  tmp_w = coerce_input_double(w,type_w,1,0,NULL,NULL);
  tmp_p = coerce_input_double(p,type_p,1,0,NULL,NULL);
  if(tmp_w == NULL || tmp_p == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_w == NCL_double || type_p == NCL_double) {
    type_tmrskewt = NCL_double;
    tmrskewt = (void*)calloc(1,sizeof(double));
  }
  else {
    type_tmrskewt = NCL_float;
    tmrskewt = (void*)calloc(1,sizeof(float));
  }
  if( tmrskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tmr_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  *tmp_tmrskewt = NGCALLF(dtmrskewt,DTMRSKEWT)(tmp_w,tmp_p);

  coerce_output_float_or_double(tmrskewt,tmp_tmrskewt,type_tmrskewt,1,0);

/*
 * Free memory.
 */
  if(type_w != NCL_double) NclFree(tmp_w);
  if(type_p != NCL_double) NclFree(tmp_p);
  NclFree(tmp_tmrskewt);

  dsizes[0] = 1;
  return(NclReturnValue(tmrskewt,1,dsizes,NULL,type_tmrskewt,0));
}

NhlErrorTypes tda_skewt_W( void )
{
/*
 * Input array variables
 */
  void *o, *p;
  double *tmp_o, *tmp_p;
  ng_size_t dsizes[1];
  NclBasicDataTypes type_o, type_p;
/*
 * Output array variables
 */
  void *tdaskewt;
  double *tmp_tdaskewt;
  NclBasicDataTypes type_tdaskewt;
/*
 * Retrieve arguments.
 */
  o = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_o,
          DONT_CARE);
  p = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
/*
 * Create temp arrays to hold subarray of o, y, and tdaskewt.
 */
  tmp_tdaskewt = (double*)calloc(1,sizeof(double));
  if(tmp_tdaskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

  tmp_o = coerce_input_double(o,type_o,1,0,NULL,NULL);
  tmp_p = coerce_input_double(p,type_p,1,0,NULL,NULL);
  if(tmp_o == NULL || tmp_p == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_o == NCL_double || type_p == NCL_double) {
    type_tdaskewt = NCL_double;
    tdaskewt = (void*)calloc(1,sizeof(double));
  }
  else {
    type_tdaskewt = NCL_float;
    tdaskewt = (void*)calloc(1,sizeof(float));
  }
  if( tdaskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tda_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  *tmp_tdaskewt = NGCALLF(dtdaskewt,DTDASKEWT)(tmp_o,tmp_p);

  coerce_output_float_or_double(tdaskewt,tmp_tdaskewt,type_tdaskewt,1,0);

/*
 * Free memory.
 */
  if(type_o != NCL_double) NclFree(tmp_o);
  if(type_p != NCL_double) NclFree(tmp_p);
  NclFree(tmp_tdaskewt);

  dsizes[0] = 1;
  return(NclReturnValue(tdaskewt,1,dsizes,NULL,type_tdaskewt,0));
}

NhlErrorTypes satlft_skewt_W( void )
{
/*
 * Input array variables
 */
  void *thw, *p;
  double *tmp_thw, *tmp_p;
  ng_size_t dsizes[1];
  NclBasicDataTypes type_thw, type_p;
/*
 * Output array variables
 */
  void *satlftskewt;
  double *tmp_satlftskewt;
  NclBasicDataTypes type_satlftskewt;
/*
 * Retrieve arguments.
 */
  thw = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_thw,
          DONT_CARE);
  p = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
/*
 * Create temp arrays to hold subarray of o, y, and satlftskewt.
 */
  tmp_satlftskewt = (double*)calloc(1,sizeof(double));
  if(tmp_satlftskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

  tmp_thw = coerce_input_double(thw,type_thw,1,0,NULL,NULL);
  tmp_p   = coerce_input_double(p,type_p,1,0,NULL,NULL);
  if(tmp_thw == NULL || tmp_p == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_thw == NCL_double || type_p == NCL_double) {
    type_satlftskewt = NCL_double;
    satlftskewt = (void*)calloc(1,sizeof(double));
  }
  else {
    type_satlftskewt = NCL_float;
    satlftskewt = (void*)calloc(1,sizeof(float));
  }
  if( satlftskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"satlft_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }
/*
 * Call Fortran routine.
 */
  *tmp_satlftskewt = NGCALLF(dsatlftskewt,DSATLFTSKEWT)(tmp_thw,tmp_p);

  coerce_output_float_or_double(satlftskewt,tmp_satlftskewt,
				type_satlftskewt,1,0);
/*
 * Free memory.
 */
  if(type_thw != NCL_double) NclFree(tmp_thw);
  if(type_p   != NCL_double) NclFree(tmp_p);
  NclFree(tmp_satlftskewt);

  dsizes[0] = 1;
  return(NclReturnValue(satlftskewt,1,dsizes,NULL,type_satlftskewt,0));
}

NhlErrorTypes ptlcl_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *td;
  double *tmp_t, *tmp_p, *tmp_td;
  NclBasicDataTypes type_t, type_p, type_td;
/*
 * Output array variables
 */
  void *pc, *tc;
  double *tmp_pc = NULL;
  double *tmp_tc = NULL;
  NclBasicDataTypes type_pc, type_tc;
/*
 * Retrieve arguments.
 */
  p = (void*)NclGetArgValue(
          0,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
  t = (void*)NclGetArgValue(
          1,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_t,
          DONT_CARE);
  td = (void*)NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_td,
          DONT_CARE);
  pc = (void*)NclGetArgValue(
          3,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_pc,
          DONT_CARE);
  tc = (void*)NclGetArgValue(
          4,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_tc,
          DONT_CARE);
/*
 * Output variables must be float or double.
 */
  if((type_pc != NCL_float && type_pc != NCL_double) ||
     (type_tc != NCL_float && type_tc != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: pc and tc must be float or double");
    return(NhlFATAL);
  }
/*
 * Create temp arrays to hold subarray of p, t, td, pc, and tc.
 */
  tmp_t  = coerce_input_double(t,type_t,1,0,NULL,NULL);
  tmp_p  = coerce_input_double(p,type_p,1,0,NULL,NULL);
  tmp_td = coerce_input_double(td,type_td,1,0,NULL,NULL);
  if(tmp_t == NULL || tmp_p == NULL || tmp_td == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

  if(type_pc == NCL_float) {
    tmp_pc = (double*)calloc(1,sizeof(double));
    if(tmp_pc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing pc to double precision");
      return(NhlFATAL);
    }
  }

  if(type_tc == NCL_float) {
    tmp_tc = (double*)calloc(1,sizeof(double));
    if(tmp_tc == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"ptlcl_skewt: Unable to allocate memory for coercing tc to double precision");
      return(NhlFATAL);
    }
  }

/*
 * Call Fortran routine.
 */
  if(type_pc == NCL_double) tmp_pc = &((double*)pc)[0];
  if(type_tc == NCL_double) tmp_tc = &((double*)tc)[0];

  NGCALLF(dptlclskewt,DPTLCLSKEWT)(tmp_p,tmp_t,tmp_td,tmp_pc,tmp_tc);

  if(type_pc != NCL_double) ((float*)pc)[0] = (float)*tmp_pc;
  if(type_tc != NCL_double) ((float*)tc)[0] = (float)*tmp_tc;
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  if(type_pc != NCL_double) NclFree(tmp_pc);
  if(type_tc != NCL_double) NclFree(tmp_tc);

  return(NhlNOERROR);
}

NhlErrorTypes showal_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *t, *td;
  double *tmp_t, *tmp_p, *tmp_td;
  ng_size_t dsizes_t[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_td[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes[1];
  NclBasicDataTypes type_t, type_p, type_td;
/*
 * Output array variables
 */
  void *showalskewt;
  double *tmp_showalskewt;
  NclBasicDataTypes type_showalskewt;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t nlvls;
  int inlvls;
/*
 * Retrieve arguments.
 */
  p = (void*)NclGetArgValue(
          0,
          3,
          NULL,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
  t = (void*)NclGetArgValue(
          1,
          3,
          NULL,
          dsizes_t,
          NULL,
          NULL,
          &type_t,
          DONT_CARE);
  td = (void*)NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_td,
          NULL,
          NULL,
          &type_td,
          DONT_CARE);
/*
 * input arrays must be the same size.
 */
  nlvls = dsizes_t[0];
  if(dsizes_p[0] != nlvls || dsizes_td[0] != nlvls) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: The input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if(nlvls > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: the length of t is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlvls = (int) nlvls;

/*
 * Create temp arrays to hold subarray of p, t, td.
 */
  tmp_showalskewt = (double*)calloc(1,sizeof(double));
  if(tmp_showalskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

  tmp_t  = coerce_input_double(t,type_t,nlvls,0,NULL,NULL);
  tmp_p  = coerce_input_double(p,type_p,nlvls,0,NULL,NULL);
  tmp_td = coerce_input_double(td,type_td,nlvls,0,NULL,NULL);
  if(tmp_t == NULL || tmp_p == NULL || tmp_td == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_t == NCL_double || type_p == NCL_double || type_td == NCL_double) {
    type_showalskewt = NCL_double;
    showalskewt = (void*)calloc(1,sizeof(double));
  }
  else {
    type_showalskewt = NCL_float;
    showalskewt = (void*)calloc(1,sizeof(float));
  }
  if( showalskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"showal_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  *tmp_showalskewt = NGCALLF(dshowalskewt,DSHOWALSKEWT)(tmp_p,tmp_t,tmp_td,
                                                        &inlvls);
  coerce_output_float_or_double(showalskewt,tmp_showalskewt,
				type_showalskewt,1,0);
/*
 * Free memory.
 */
  if(type_t  != NCL_double) NclFree(tmp_t);
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  NclFree(tmp_showalskewt);

  dsizes[0] = 1;
  return(NclReturnValue(showalskewt,1,dsizes,NULL,type_showalskewt,0));
}

NhlErrorTypes pw_skewt_W( void )
{
/*
 * Input array variables
 */
  void *p, *td;
  double *tmp_p, *tmp_td;
  ng_size_t dsizes_p[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_td[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes[1];
  NclBasicDataTypes type_p, type_td;
/*
 * Output array variables
 */
  void *pwskewt;
  double *tmp_pwskewt;
  NclBasicDataTypes type_pwskewt;
/*
 * Declare various variables for random purposes.
 */
  ng_size_t n;
  int in;
/*
 * Retrieve arguments.
 */
  td = (void*)NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_td,
          NULL,
          NULL,
          &type_td,
          DONT_CARE);
  p = (void*)NclGetArgValue(
          1,
          2,
          NULL,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);
/*
 * input arrays must be the same size.
 */
  if(dsizes_td[0] != dsizes_p[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: The input arrays must have the same dimension sizes");
    return(NhlFATAL);
  }
  n = dsizes_td[0];
/*
 * Test dimension sizes.
 */
  if(n > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: the length of td is greater than INT_MAX");
    return(NhlFATAL);
  }
  in = (int) n;

/*
 * Create temp arrays to hold subarray of p, t, td.
 */
  tmp_pwskewt = (double*)calloc(1,sizeof(double));
  if(tmp_pwskewt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

  tmp_p  = coerce_input_double(p,type_p,n,0,NULL,NULL);
  tmp_td = coerce_input_double(td,type_td,n,0,NULL,NULL);

  if(tmp_p == NULL || tmp_td == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_td == NCL_double || type_p == NCL_double) {
    type_pwskewt = NCL_double;
    pwskewt = (void*)calloc(1,sizeof(double));
  }
  else {
    type_pwskewt = NCL_float;
    pwskewt = (void*)calloc(1,sizeof(float));
  }
  if( pwskewt == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"pw_skewt: Unable to allocate memory for output");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  *tmp_pwskewt = NGCALLF(dpwskewt,DPWSKEWT)(tmp_td,tmp_p,&in);
  coerce_output_float_or_double(pwskewt,tmp_pwskewt,type_pwskewt,1,0);

/*
 * Free memory.
 */
  if(type_p  != NCL_double) NclFree(tmp_p);
  if(type_td != NCL_double) NclFree(tmp_td);
  NclFree(tmp_pwskewt);

  dsizes[0] = 1;
  return(NclReturnValue(pwskewt,1,dsizes,NULL,type_pwskewt,0));
}

NhlErrorTypes cape_thermo_W( void )
{
/*
 * Input array variables
 */
  void *penv, *tenv, *lclmb;
  int *iprint;
  double *tmp_tenv, *tmp_penv, *tmp_lclmb, *tmp_cape;
  NclScalar missing_tenv, missing_dtenv, missing_rtenv;
  ng_size_t dsizes_tenv[NCL_MAX_DIMENSIONS];
  int has_missing_tenv;
  ng_size_t dsizes_penv[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes[1];
  NclBasicDataTypes type_tenv, type_penv, type_lclmb;
/*
 * Output array variables
 */
  void *cape;
  NclBasicDataTypes type_cape;
/*
 * Attribute variables
 */
  int att_id;
  NclMultiDValData att_md, return_md;
  NclVar tmp_var;
  NclStackEntry return_data;
/*
 * Declare various variables for random purposes.
 */
  double *tparcel = NULL;
  float *rtparcel = NULL;
  int *jlcl, *jlfc, *jcross;
  ng_size_t nlvls;
  int inlvls;
/*
 * Retrieve arguments.
 */
  penv = (void*)NclGetArgValue(
          0,
          4,
          NULL,
          dsizes_penv,
          NULL,
          NULL,
          &type_penv,
          DONT_CARE);
  tenv = (void*)NclGetArgValue(
          1,
          4,
          NULL,
          dsizes_tenv,
          &missing_tenv,
          &has_missing_tenv,
          &type_tenv,
          DONT_CARE);
  lclmb = (void*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_lclmb,
          DONT_CARE);
  iprint = (int*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);
/*
 * tenv and penv must be the same size.
 */
  if(dsizes_tenv[0] !=  dsizes_penv[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: tenv and penv must have the same dimension sizes");
    return(NhlFATAL);
  }
  nlvls = dsizes_tenv[0];

/*
 * Test dimension sizes.
 */
  if(nlvls > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: the length of tenv is greater than INT_MAX");
    return(NhlFATAL);
  }
  inlvls = (int) nlvls;

/*
 * Check for missing values.
 */
  coerce_missing(type_tenv,has_missing_tenv,&missing_tenv,&missing_dtenv,
                 &missing_rtenv);
/*
 * Create temp arrays to hold subarray of penv, tenv, lclmb.
 */
  tmp_tenv  = coerce_input_double(tenv,type_tenv,nlvls,0,NULL,NULL);
  tmp_penv  = coerce_input_double(penv,type_penv,nlvls,0,NULL,NULL);
  tmp_lclmb = coerce_input_double(lclmb,type_lclmb,1,0,NULL,NULL);

  if(tmp_tenv == NULL || tmp_penv == NULL || tmp_lclmb == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to coerce input to double precision");
    return(NhlFATAL);
  }

/*
 * Allocate space for output array
 */
  if(type_tenv == NCL_double || type_penv == NCL_double) {
    type_cape = NCL_double;
    cape      = (void*)calloc(1,sizeof(double));
    tmp_cape  = (double*)calloc(1,sizeof(double));
    if( tmp_cape == NULL || cape == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output");
      return(NhlFATAL);
    }
  }
  else {
    type_cape = NCL_float;
    rtparcel  = (float*)calloc(nlvls,sizeof(float));
    cape      = (void*)calloc(1,sizeof(float));
    tmp_cape = (double*)calloc(1,sizeof(double));
    if( tmp_cape == NULL || cape == NULL || rtparcel == NULL ) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output");
      return(NhlFATAL);
    }
  }
/*
 * Allocate space for dummy variables.
 */
  tparcel = (double*)calloc(nlvls,sizeof(double));
  jlcl    = (int*)calloc(1,sizeof(int));
  jlfc    = (int*)calloc(1,sizeof(int));
  jcross  = (int*)calloc(1,sizeof(int));
  if(tparcel == NULL || jlcl == NULL || jlfc == NULL || jcross == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"cape_thermo: Unable to allocate memory for output variables");
    return(NhlFATAL);
  }

/*
 * Call Fortran routine.
 */
  *tmp_cape = NGCALLF(dcapethermo,DCAPETHERMO)(tmp_penv,tmp_tenv,
                                               &inlvls,tmp_lclmb,
                                               iprint,tparcel,
                                               &missing_dtenv.doubleval,
                                               jlcl,jlfc,jcross);

  coerce_output_float_or_double(cape,tmp_cape,type_cape,1,0);

  if(type_cape != NCL_double) {
    coerce_output_float_only(rtparcel,tparcel,nlvls,0);
    NclFree(tparcel);
  }
/*
 * Free memory.
 */
  if(type_tenv  != NCL_double) NclFree(tmp_tenv);
  if(type_penv  != NCL_double) NclFree(tmp_penv);
  if(type_lclmb != NCL_double) NclFree(tmp_lclmb);
  NclFree(tmp_cape);

  if(type_cape == NCL_double) {
    dsizes[0] = 1;
    return_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,cape,NULL,1,
                              dsizes,TEMPORARY,NULL,
                              (NclObjClass)nclTypedoubleClass);
/*
 * Set up double attribute to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    dsizes[0] = nlvls;
    att_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,tparcel,NULL,1,
                           dsizes,TEMPORARY,NULL,
                           (NclObjClass)nclTypedoubleClass);
  }
  else {
    dsizes[0] = 1;
    return_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,cape,NULL,1,
                              dsizes,TEMPORARY,NULL,
                              (NclObjClass)nclTypefloatClass);
/*
 * Set up float attribute to return.
 */
    att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
    dsizes[0] = nlvls;
    att_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,rtparcel,NULL,1,
                           dsizes,TEMPORARY,NULL,
                           (NclObjClass)nclTypefloatClass);
  }
  _NclAddAtt(att_id,"tparcel",att_md,NULL);

  dsizes[0] = 1;
  att_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,jlcl,NULL,1,
                         dsizes,TEMPORARY,NULL,
                         (NclObjClass)nclTypeintClass);
  _NclAddAtt(att_id,"jlcl",att_md,NULL);

  att_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,jlfc,NULL,1,
                         dsizes,TEMPORARY,NULL,
                         (NclObjClass)nclTypeintClass);
  _NclAddAtt(att_id,"jlfc",att_md,NULL);

  att_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,jcross,NULL,1,
                         dsizes,TEMPORARY,NULL,
                         (NclObjClass)nclTypeintClass);
  _NclAddAtt(att_id,"jcross",att_md,NULL);

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


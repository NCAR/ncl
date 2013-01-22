#include <stdio.h>
#include <string.h>
#include "wrapper.h"

int  fterr;
char ftmsg[61];


NhlErrorTypes ftsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"sf1", "sf2", "df1", "df2", "df3",
                      "df4", "df5", "df6", "df7", "df8",
                      "SF1", "SF2", "DF1", "DF2", "DF3",
                      "DF4", "DF5", "DF6", "DF7", "DF8"};
  char *params_f[] = {"sig", "sl1", "sln", "smt", "eps", 
                      "SIG", "SL1", "SLN", "SMT", "EPS"};
  char *params_c[] = {"dum", "DUM"};

/*
 * Input array variables
 */
  NrmQuark *pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname, type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpc = sizeof(params_c)/sizeof(void *);
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      goto OK_NAME;
    } 
  }
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      goto OK_NAME;
    } 
  }
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      goto OK_NAME;
    } 
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "ftsetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 * Retrieve argument #2
 */
OK_NAME:  pvalue = (void *) NclGetArgValue(
           1,
           2,
           NULL,
           dsizes_pvalue,
           NULL,
           NULL,
           &type_pvalue,
           DONT_CARE);

/*
 *  Process the parameter if it has an integer value.
 */
  if (type_pvalue == NCL_int) {
    for (i = 0; i < numpi; i++) {
      if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
        c_ftseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ftsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_ftsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_ftsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ftsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((NrmQuark *) pvalue));
        c_ftsetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ftsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ftsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
}

NhlErrorTypes ftgetp_W(void)
{
/*
 *  Get values for fitpack parameters.
 */

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;
  NrmQuark *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"sf1", "sf2", "SF1", "SF2"};
  char *params_f[] = {"sig", "sl1", "sln", "smt", "eps", 
                      "SIG", "SL1", "SLN", "SMT", "EPS"};
  char *params_c[] = {"dum", "DUM"};

/*
 * Input array variable
 */
  NrmQuark *pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname;
  float *fval;
  int *ival;
  ng_size_t ret_size = 1;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          1,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpc = sizeof(params_c)/sizeof(void *);
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      goto OK_NAME;
    } 
  }
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      goto OK_NAME;
    } 
  }
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      goto OK_NAME;
    } 
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "ftgetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_ftgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_ftgetr(arg1, fval);
      return(NclReturnValue((void *) fval, 1, &ret_size, NULL, NCL_float, 0));
    }
  }

/*
 *  Process the parameter if it has a string value.
 */
  for (i = 0; i < numpc; i++) {
    if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
      cval = (char *) calloc(100,sizeof(char));
      if (cval == NULL) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
             "ftgetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_ftgetc(arg1, cval);
      qvalue = (NrmQuark *) calloc(1,sizeof(NrmQuark));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "ftgetp: impossible to get this message");
  return(NhlFATAL);
}

NhlErrorTypes ftcurv_W(void)
{

/*
 * Input array variables
 */
  void *xi, *yi, *xo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  void *yo;
  ng_size_t *dsizes_yo;
/*
 * Various
 */
  int inpts, inxo, ret;
  ng_size_t i, npts, nxo, size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo;

/*
 * Retrieve xi.
 */
  xi = (void *) NclGetArgValue(
          0,
          3,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurv: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi.
 */
  yi = (void *) NclGetArgValue(
          1,
          3,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then it must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurv: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurv: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurv: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }


/*
 * Retrieve xo.
 */
  xo = (float *) NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurv: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coerce input and allocate memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xo != NCL_double) {
    type_yo = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(float));
  }
  else {
    type_yo = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(double));
  }
  dsizes_yo = (ng_size_t *) calloc(ndims_yi, sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurv: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurv: Unable to coerce xo to double");
    return(NhlFATAL);
  }
  if( yo == NULL || dsizes_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurv: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 * Loop through leftmost points and call C version of routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }

    if(type_yo == NCL_float) {
      tmp_yo = &((float*)yo) [index_out];
      fterr = c_ftcurv(inpts, tmp_xi, tmp_yi, inxo, tmp_xo, tmp_yo);
    }
    else {
      tmp_yo = &((double*)yo) [index_out];
      fterr = c_ftcurvdp(inpts, tmp_xi, tmp_yi, inxo, tmp_xo, tmp_yo);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurv: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }
/*
 * Free up memory we don't need.
 */
  if(type_xo != type_yo) NclFree(tmp_xo);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,type_yo,0 );
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes ftcurvd_W(void)
{
/*
 * Input array variables
 */
  void *xi, *yi, *xo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  void *yo;
  ng_size_t *dsizes_yo;
/*
 * Various
 */
  ng_size_t i, size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;
  ng_size_t npts, nxo;
  int inpts, inxo, ret;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo;

/*
 * Retrieve xi.
 */
  xi = (void *) NclGetArgValue(
          0,
          3,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvd: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi.
 */
  yi = (void *) NclGetArgValue(
          1,
          3,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvd: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvd: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvd: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve xo.
 */
  xo = (void *) NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvd: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xo != NCL_double) {
    type_yo = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(float));
  }
  else {
    type_yo = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(double));
  }
  dsizes_yo = (ng_size_t *) calloc(ndims_yi, sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvd: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvd: Unable to coerce xo to double");
    return(NhlFATAL);
  }
  if( yo == NULL || dsizes_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvd: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 * Loop through leftmost points and call C version of routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }

    if(type_yo == NCL_float) {
      tmp_yo = &((float*)yo) [index_out];
      fterr = c_ftcurvd(inpts, tmp_xi, tmp_yi, inxo, tmp_xo, tmp_yo);
    }
    else {
      tmp_yo = &((double*)yo) [index_out];
      fterr = c_ftcurvddp(inpts, tmp_xi, tmp_yi, inxo, tmp_xo, tmp_yo);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvd: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }

/*
 * Free up memory we don't need.
 */
  if(type_xo != type_yo) NclFree(tmp_xo);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,type_yo,0 );
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes ftcurvi_W(void)
{

/*
 * Input variables.
 */
  void *xl, *xr, *xi, *yi;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  void *integral;
  ng_size_t *dsizes_int;
  int ndims_int;

/*
 * Various
 */
  int inpts, ret;
  ng_size_t i, npts, size_leftmost, index_xi = 0, index_yi = 0;
  void *tmp_xi, *tmp_yi, *tmp_xl, *tmp_xr, *tmp_int;
  NclBasicDataTypes type_xi, type_yi, type_xl, type_xr, type_int;

/*
 * Retrieve argument #1
 */
  xl = (void *) NclGetArgValue(
          0,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_xl,
          DONT_CARE);

/*
 * Retrieve argument #2
 */
  xr = (void *) NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_xr,
          DONT_CARE);

/*
 * Retrieve xi.
 */
  xi = (void *) NclGetArgValue(
          2,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvi: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi.
 */
  yi = (void *) NclGetArgValue(
          3,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvi: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvi: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvi: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xl != NCL_double && type_xr != NCL_double) {
    type_int = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xl = coerce_input_float(xl, type_xl, 1, 0, NULL, NULL);
    tmp_xr = coerce_input_float(xr, type_xr, 1, 0, NULL, NULL);
    integral = (void *) calloc(size_leftmost,sizeof(float));
  }
  else {
    type_int = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xl = coerce_input_double(xl, type_xl, 1, 0, NULL, NULL);
    tmp_xr = coerce_input_double(xr, type_xr, 1, 0, NULL, NULL);
    integral = (void *) calloc(size_leftmost,sizeof(double));
  }
  ndims_int  = max(1,ndims_yi-1);
  dsizes_int = (ng_size_t *) calloc(ndims_int,sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvi: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xl == NULL || tmp_xr == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvi: Unable to coerce xl or xr to double");
    return(NhlFATAL);
  }
  if(integral == NULL || dsizes_int == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvi: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  if(ndims_int == 1) {
    dsizes_int[0] = 1;
  }
  else {
    for( i = 0; i < ndims_yi-1; i++ ) dsizes_int[i] = dsizes_yi[i];
  }

/*
 *  Loop through leftmost points and calculate the integral values.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_int == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_int == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }

    if(type_int == NCL_float) {
      tmp_int = &((float*)integral) [i];
      fterr = c_ftcurvi(*((float*)tmp_xl),*((float*)tmp_xr),inpts,tmp_xi,
                        tmp_yi,tmp_int);
    }
    else {
      tmp_int = &((double*)integral) [i];
      fterr = c_ftcurvidp(*((double*)tmp_xl),*((double*)tmp_xr),inpts,tmp_xi,
                          tmp_yi,tmp_int);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvi: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
  }

/*
 * Free up memory we don't need.
 */
  if(type_xr != type_int) NclFree(tmp_xr);
  if(type_xl != type_int) NclFree(tmp_xl);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)integral,ndims_int,dsizes_int,NULL,type_int,0 );
  NclFree(dsizes_int);
  return(ret);
}

NhlErrorTypes ftcurvp_W(void)
{

/*
 * Input array variables
 */
  void *xi, *yi, *xo, *p;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  void *yo;
  ng_size_t *dsizes_yo;

/*
 * Various
 */
  ng_size_t i, npts, nxo;
  ng_size_t size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;
  int inpts, inxo, ret;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_p;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_p;

/*
 * Retrieve xi (X coordinate input values)
 */
  xi = (void *) NclGetArgValue(
          0,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvp: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi (Y coordinate input values)
 */
  yi = (void *) NclGetArgValue(
          1,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvp: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvp: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvp: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #3 (The period)
 */
  p = (void *) NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

/*
 * Retrieve xo.
 */
  xo = (void *) NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvp: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xo != NCL_double && type_p != NCL_double) {
    type_yo = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_p  = coerce_input_float(p, type_p, 1, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(float));
  }
  else {
    type_yo = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_p  = coerce_input_double(p, type_p, 1, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(double));
  }
  dsizes_yo = (ng_size_t *) calloc(ndims_yi, sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvp: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xo == NULL || tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvp: Unable to coerce xo or p to double");
    return(NhlFATAL);
  }
  if( yo == NULL || dsizes_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvp: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 * Loop through leftmost points and call C version of routine.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }
    if(type_yo == NCL_float) {
      tmp_yo = &((float*)yo) [index_out];
      fterr = c_ftcurvp(inpts, tmp_xi, tmp_yi, *((float*)tmp_p), inxo, 
                        tmp_xo, tmp_yo);
    }
    else {
      tmp_yo = &((double*)yo) [index_out];
      fterr = c_ftcurvpdp(inpts, tmp_xi, tmp_yi, *((double*)tmp_p), inxo, 
                          tmp_xo, tmp_yo);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvp: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }

/*
 * Free up memory we don't need.
 */
  if(type_yo != type_xo) NclFree(tmp_xo);
  if(type_yo != type_p)  NclFree(tmp_p);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,type_yo,0 );
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes ftcurvpi_W(void)
{

/*
 * Input variables.
 */
  void *xl, *xr, *xi, *yi, *p;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];

/*
 * Output variables.
 */
  void *integral;
  ng_size_t *dsizes_int;
  int ndims_int;

/*
 * Various
 */
  ng_size_t i, npts, size_leftmost, index_xi = 0, index_yi = 0;
  int inpts, ret;
  void *tmp_xi, *tmp_yi, *tmp_xl, *tmp_xr, *tmp_int, *tmp_p;
  NclBasicDataTypes type_xi, type_yi, type_xl, type_xr, type_int, type_p;

/*
 * Retrieve argument #1 (left integral limit).
 */
  xl = (void *) NclGetArgValue(
          0,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_xl,
          DONT_CARE);

/*
 * Retrieve argument #2 (right integral limit).
 */
  xr = (void *) NclGetArgValue(
          1,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_xr,
          DONT_CARE);

/*
 * Retrieve argument #3 (The period)
 */
  p = (void *) NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

/*
 * Retrieve xi (the X coordinate input values).
 */
  xi = (void *) NclGetArgValue(
          3,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvpi: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi (the Y coordinate input values).
 */
  yi = (void *) NclGetArgValue(
          4,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvpi: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvpi: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvpi: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xl != NCL_double && type_xr != NCL_double && type_p != NCL_double) {
    type_int = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xl = coerce_input_float(xl, type_xl, 1, 0, NULL, NULL);
    tmp_xr = coerce_input_float(xr, type_xr, 1, 0, NULL, NULL);
    tmp_p  = coerce_input_float(p, type_p, 1, 0, NULL, NULL);
    integral = (void *) calloc(size_leftmost,sizeof(float));
  }
  else {
    type_int = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xl = coerce_input_double(xl, type_xl, 1, 0, NULL, NULL);
    tmp_xr = coerce_input_double(xr, type_xr, 1, 0, NULL, NULL);
    tmp_p  = coerce_input_double(p, type_p, 1, 0, NULL, NULL);
    integral = (void *) calloc(size_leftmost,sizeof(double));
  }
  ndims_int  = max(1,ndims_yi-1);
  dsizes_int = (ng_size_t *) calloc(ndims_int,sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvpi: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xl == NULL || tmp_xr == NULL || tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvpi: Unable to coerce xl or xr to double");
    return(NhlFATAL);
  }
  if(integral == NULL || dsizes_int == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvpi: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  if(ndims_int == 1) {
    dsizes_int[0] = 1;
  }
  else {
    for( i = 0; i < ndims_yi-1; i++ ) dsizes_int[i] = dsizes_yi[i];
  }

/*
 *  Loop through leftmost points and calculate the integral value.
 */
  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_int == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_int == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }

    if(type_int == NCL_float) {
      tmp_int = &((float*)integral) [i];
      fterr = c_ftcurvpi(*((float*)tmp_xl),*((float*)tmp_xr),
                         *((float*)tmp_p),inpts,tmp_xi,tmp_yi,tmp_int);
    }
    else {
      tmp_int = &((double*)integral) [i];
      fterr = c_ftcurvpidp(*((double*)tmp_xl),*((double*)tmp_xr),
                           *((double*)tmp_p),inpts,tmp_xi,tmp_yi,tmp_int);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvpi: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    if(ndims_xi > 1) index_xi += npts;
    index_yi += npts;
  }

/*
 * Free up memory we don't need.
 */
  if(type_xr != type_int) NclFree(tmp_xr);
  if(type_xl != type_int) NclFree(tmp_xl);
  if(type_p  != type_int) NclFree(tmp_p);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)integral,ndims_int,dsizes_int,NULL,
                        type_int,0 );
  NclFree(dsizes_int);
  return(ret);
}

NhlErrorTypes ftcurvs_W(void)
{

/*
 * Input variables.
 */
  void *xi, *yi, *xo, *d;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_d[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  void *yo;
  ng_size_t *dsizes_yo;

/*
 * Various
 */
  ng_size_t i, size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;
  ng_size_t npts, nxo;
  int inpts, inxo;
  int isw, ret;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_d;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_d;

/*
 * Retrieve xi (X coordinate input points).
 */
  xi = (void *) NclGetArgValue(
          0,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvs: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi (Y coordinate input values).
 */
  yi = (void *) NclGetArgValue(
          1,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvs: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvs: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvs: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #2 (The observation weights).
 */
  d = (void *) NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_d,
          NULL,
          NULL,
          &type_d,
          DONT_CARE);

  if(dsizes_d[0] != npts && dsizes_d[0] != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: The d must be the same length as xi's last dimension or should be scalar");
    return(NhlFATAL);
  }

/*
 * Retrieve xo (the X coordinate output values).
 */
  xo = (void *) NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvs: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && type_xo != NCL_double
         && type_d != NCL_double ) {
    type_yo = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_d  = coerce_input_float(d, type_d, dsizes_d[0], 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(float));
  }
  else {
    type_yo = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_d  = coerce_input_double(d, type_d, dsizes_d[0], 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(double));
  }
  dsizes_yo = (ng_size_t *) calloc(   ndims_yi, sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvs: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xo == NULL || tmp_d == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvs: Unable to coerce xo or s to double");
    return(NhlFATAL);
  }
  if( yo == NULL || dsizes_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvs: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Loop through leftmost points and calculate the interpolated smooth curve.
 */
  isw = 1;
  if (dsizes_d[0] > 1) isw = 0;

  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }
    if(type_yo == NCL_float) {
      tmp_yo = &((float*)yo) [index_out];
      fterr = c_ftcurvs(inpts,tmp_xi,tmp_yi,isw,tmp_d,inxo,tmp_xo,tmp_yo);
    }
    else {
      tmp_yo = &((double*)yo) [index_out];
      fterr = c_ftcurvsdp(inpts,tmp_xi,tmp_yi,isw,tmp_d,inxo,tmp_xo,tmp_yo);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvs: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }

/*
 * Free up memory we don't need.
 */
  if(type_xo != type_yo) NclFree(tmp_xo);
  if(type_d  != type_yo) NclFree(tmp_d);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,type_yo,0 );
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes ftcurvps_W(void)
{

/*
 * Input variables.
 */
  void *xi, *yi, *xo, *p, *d;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_d[1];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];

/*
 * Output variables.
 */
  void *yo;
  ng_size_t *dsizes_yo;

/*
 * Various
 */
  ng_size_t i, size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;
  ng_size_t npts, nxo;
  int inpts, inxo;
  int isw, ret;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_d, *tmp_p;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_d, type_p;

/*
 * Retrieve xi (X coordinate input points).
 */
  xi = (void *) NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[ndims_xi-1] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvs: the rightmost dimension of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[ndims_xi-1];
  inpts = (int) npts;

/*
 * Retrieve yi (Y coordinate input values).
 */
  yi = (void *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * If xi is not 1-dimensional, then is must be the same size as yi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_yi) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvps: If xi is not 1-dimensional, then it must be the same size as yi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi; i++) {
      if(dsizes_xi[i] != dsizes_yi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftcurvps: If xi is not 1-dimensional, then it must have the same dimension sizes as yi");
        return(NhlFATAL);
      }
    }
  }
  else {
    if(dsizes_yi[ndims_yi-1] != npts) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftcurvps: The last dimension of yi must be the same length as xi");
      return(NhlFATAL);
    }
  }

/*
 * Retrieve argument #2 (The period).
 */
  p = (void *) NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          &type_p,
          DONT_CARE);

/*
 * Retrieve argument #3 (The observation weights).
 */
  d = (void *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_d,
          NULL,
          NULL,
          &type_d,
          DONT_CARE);

  if(dsizes_d[0] != npts && dsizes_d[0] != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: d must be a scalar or the same length as the rightmost dimension of xi");
    return(NhlFATAL);
  }

/*
 * Retrieve xo (the X coordinate output values).
 */
  xo = (void *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xo[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftcurvps: the length of xo is greater than INT_MAX");
    return(NhlFATAL);
  }
  nxo  = dsizes_xo[0];
  inxo = (int) nxo;

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Coercing inputs and allocating memory for output arrays.
 */
  if(type_xi != NCL_double && type_yi != NCL_double && type_xo != NCL_double
     && type_p != NCL_double && type_d != NCL_double ) {
    type_yo = NCL_float;
    tmp_xi = (float*) calloc(npts, sizeof(float));
    tmp_yi = (float*) calloc(npts, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_d  = coerce_input_float(d, type_d, dsizes_d[0], 0, NULL, NULL);
    tmp_p  = coerce_input_float(p, type_p, 1, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(float));
  }
  else {
    type_yo = NCL_double;
    tmp_xi = (double*) calloc(npts, sizeof(double));
    tmp_yi = (double*) calloc(npts, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_d  = coerce_input_double(d, type_d, dsizes_d[0], 0, NULL, NULL);
    tmp_p  = coerce_input_double(p, type_p, 1, 0, NULL, NULL);
    yo = (void *) calloc(size_leftmost*nxo, sizeof(double));
  }
  dsizes_yo = (ng_size_t *) calloc(ndims_yi, sizeof(ng_size_t));

  if( tmp_xi == NULL || tmp_yi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvps: Unable to allocate memory for coercing xi or yi array to float");
    return(NhlFATAL);
  }
  if( tmp_xo == NULL || tmp_d == NULL || tmp_p == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftcurvps: Unable to coerce xo or s or p to double");
    return(NhlFATAL);
  }
  if( yo == NULL || dsizes_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftcurvps: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Loop through leftmost points and calculate the interpolated smooth curve.
 */
  isw = 1;
  if (dsizes_d[0] > 1) isw = 0;

  for( i = 0; i < size_leftmost; i++ ) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
    if(ndims_xi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,npts,
                                   0,NULL,NULL);
      }
    }
/*
 * Coerce yi only once if it is one-dimensional.
 */
    if(ndims_yi > 1 || !i) {
      if(type_yo == NCL_float ) {
        coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,npts,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,npts,
                                   0,NULL,NULL);
      }
    }
    if(type_yo == NCL_float) {
      tmp_yo = &((float*)yo) [index_out];
      fterr = c_ftcurvps(inpts,tmp_xi,tmp_yi,*((float*)tmp_p),isw,tmp_d,
                         inxo,tmp_xo,tmp_yo);
    }
    else {
      tmp_yo = &((double*)yo) [index_out];
      fterr = c_ftcurvpsdp(inpts,tmp_xi,tmp_yi,*((double*)tmp_p),isw,tmp_d,
                           inxo,tmp_xo,tmp_yo);
    }
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvps: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }

/*
 * Free up memory we don't need.
 */
  if(type_xo != type_yo) NclFree(tmp_xo);
  if(type_d  != type_yo) NclFree(tmp_d);
  if(type_p  != type_yo) NclFree(tmp_p);
  NclFree(tmp_xi);
  NclFree(tmp_yi);

  ret = NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,type_yo,0 );
  NclFree(dsizes_yo);
  return(ret);
}

NhlErrorTypes ftkurv_W(void)
{
/*
 * Input array variables
 */
  void *xi, *yi, *xo, *ti, *yo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_ti[1], dsizes_xo[1], dsizes_yo[1];
  ng_size_t npts, mpts;
  int inpts, impts;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_ti;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_ti, type_tmp;

/*
 * Retrieve argument #1
 */
  xi = (void *) NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurv: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2
 */
  yi = (void *) NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * Check dimension of argument #1.
 */
  if(dsizes_yi[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3
 */
  ti = (void *) NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_ti[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurv: the length of ti is greater than INT_MAX");
    return(NhlFATAL);
  }
  mpts  = dsizes_ti[0];
  impts = (int) mpts;

/*
 * Retrieve argument #4
 */
  xo = (void *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Retrieve argument #5
 */
  yo = (void *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

/*
 * Check dimension of arguments #2, 3, and #4.
 */
  if(dsizes_xo[0] != mpts || dsizes_yo[0] != mpts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Arguments #2, 3, and #4 must be the same length");
    return(NhlFATAL);
  }

/*
 * Check types of arguments #3, and #4.
 */
  if(type_xo != NCL_double && type_xo != NCL_float ) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: xo must be of type float or double");
    return(NhlFATAL);
  }
  if(type_yo != NCL_double && type_yo != NCL_float ) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: yo must be of type float or double");
    return(NhlFATAL);
  }

/*
 * Coerce input.  If any of the input is double, then promote everything
 * to double and call the double precision version of the routine.
 * Otherwise, promote everything to float and call the float routine.
 */
  if(type_xi == NCL_double || type_yi == NCL_double ||
     type_ti == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double ) {

    type_tmp = NCL_double;

    tmp_xi = coerce_input_double(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_double(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_double(ti, type_ti, mpts, 0, NULL, NULL);

    if(type_xo == NCL_float) {
      tmp_xo = (double*) calloc(mpts, sizeof(double));
      if( tmp_xo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurv: Unable to allocate memory for xo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xo = &((double*)xo) [0];
    }
    if(type_yo == NCL_float) {
      tmp_yo = (double*) calloc(mpts, sizeof(double));
      if( tmp_yo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurv: Unable to allocate memory for yo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yo = &((double*)yo) [0];
    }

  }
  else {
    type_tmp = NCL_float;

    tmp_xi = coerce_input_float(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_float(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_float(ti, type_ti, mpts, 0, NULL, NULL);
    tmp_xo = &((float*)xo) [0];
    tmp_yo = &((float*)yo) [0];
  }

  if( tmp_xi == NULL || tmp_yi == NULL || tmp_ti == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurv: Unable to coerce xi or yi or ti to double or float");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */
  if(type_tmp == NCL_float) {
    fterr = c_ftkurv(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo);
  }
  else {
    fterr = c_ftkurvdp(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo);

    if(type_xo == NCL_float) {
      coerce_output_float_only(xo,tmp_xo,mpts,0);
    }
    if(type_yo == NCL_float) {
      coerce_output_float_only(yo,tmp_yo,mpts,0);
    }
  }
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurv: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }

/*
 * Free up memory we don't need.
 */
  if(type_tmp == NCL_float) {
    if(type_xi != NCL_float) NclFree(tmp_xi);
    if(type_yi != NCL_float) NclFree(tmp_yi);
    if(type_ti != NCL_float) NclFree(tmp_ti);
  }
  else {
    if(type_xi != NCL_double) NclFree(tmp_xi);
    if(type_yi != NCL_double) NclFree(tmp_yi);
    if(type_ti != NCL_double) NclFree(tmp_ti);
    if(type_xo != NCL_double) NclFree(tmp_xo);
    if(type_yo != NCL_double) NclFree(tmp_yo);
  }

  return(NhlNOERROR);
}

NhlErrorTypes ftkurvp_W(void)
{

/*
 * Input array variables
 */
  void *xi, *yi, *xo, *ti, *yo;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_ti[1], dsizes_xo[1], dsizes_yo[1];

  ng_size_t npts, mpts;
  int inpts, impts;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_ti;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_ti, type_tmp;

/*
 * Retrieve argument #0
 */
  xi = (void *) NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvp: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;


/*
 * Retrieve argument #1
 */
  yi = (void *) NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);

/*
 * Check dimension of argument #4.
 */
  if(dsizes_yi[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2
 */
  ti = (void *) NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_ti[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvp: the length of ti is greater than INT_MAX");
    return(NhlFATAL);
  }
  mpts  = dsizes_ti[0];
  impts = (int) mpts;

/*
 * Retrieve argument #3
 */
  xo = (void *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Retrieve argument #4
 */
  yo = (void *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

/*
 * Check dimension of arguments #2, 3, and #4.
 */
  if(dsizes_xo[0] != mpts || dsizes_yo[0] != mpts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Arguments #2, 3, and #4 must be the same length");
    return(NhlFATAL);
  }

/*
 * Coerce input and create space for output if necessary. If
 * any of the input is double, then promote everything to double
 * and call the double precision version of the routine.
 * Otherwise, promote everything to float and call the float routine.
 */
  if(type_xi == NCL_double || type_yi == NCL_double ||
     type_ti == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double ) {

    type_tmp = NCL_double;

    tmp_xi = coerce_input_double(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_double(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_double(ti, type_ti, mpts, 0, NULL, NULL);

    if(type_xo == NCL_float) {
      tmp_xo = (double*) calloc(mpts, sizeof(double));
      if( tmp_xo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvp: Unable to allocate memory for xo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xo = &((double*)xo) [0];
    }

    if(type_yo == NCL_float) {
      tmp_yo = (double*) calloc(mpts, sizeof(double));
      if( tmp_yo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvp: Unable to allocate memory for yo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yo = &((double*)yo) [0];
    }
  }
  else {
    type_tmp = NCL_float;

    tmp_xi = coerce_input_float(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_float(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_float(ti, type_ti, mpts, 0, NULL, NULL);
    tmp_xo = &((float*)xo) [0];
    tmp_yo = &((float*)yo) [0];
  }

  if( tmp_xi == NULL || tmp_yi == NULL || tmp_ti == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvp: Unable to coerce xi or yi or ti to double or float");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */

  if(type_tmp == NCL_float) {
    fterr = c_ftkurvp(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo);
  }
  else {
    fterr = c_ftkurvpdp(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo);

    if(type_xo == NCL_float) {
      coerce_output_float_only(xo,tmp_xo,mpts,0);
    }
    if(type_yo == NCL_float)  {
      coerce_output_float_only(yo,tmp_yo,mpts,0);
    }
  }
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvp: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }

/*
 * Free up memory we don't need.
 */
  if(type_tmp == NCL_float) {
    if(type_xi != NCL_float) NclFree(tmp_xi);
    if(type_yi != NCL_float) NclFree(tmp_yi);
    if(type_ti != NCL_float) NclFree(tmp_ti);
  }
  else {
    if(type_xi != NCL_double) NclFree(tmp_xi);
    if(type_yi != NCL_double) NclFree(tmp_yi);
    if(type_ti != NCL_double) NclFree(tmp_ti);
    if(type_xo != NCL_double) NclFree(tmp_xo);
    if(type_yo != NCL_double) NclFree(tmp_yo);
  }

  return(NhlNOERROR);
}

NhlErrorTypes ftkurvd_W(void)
{

/*
 * Input array variables
 */
  void *xi, *yi, *xo, *ti, *yo, *xd, *yd, *xdd, *ydd;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_ti[1];
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  ng_size_t dsizes_xd[1], dsizes_yd[1];
  ng_size_t dsizes_xdd[1], dsizes_ydd[1];

  ng_size_t npts, mpts;
  int inpts, impts;
  void *tmp_xi,*tmp_yi,*tmp_xo,*tmp_yo,*tmp_ti;
  void *tmp_xd,*tmp_yd,*tmp_xdd,*tmp_ydd;
  NclBasicDataTypes type_xi,type_yi,type_xo,type_yo,type_ti,type_tmp;
  NclBasicDataTypes type_xd,type_yd,type_xdd,type_ydd;

/*
 * Retrieve argument #0
 */
  xi = (void *) NclGetArgValue(0,9,NULL,dsizes_xi,NULL,NULL,&type_xi,DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #1
 */
  yi = (void *) NclGetArgValue(1,9,NULL,dsizes_yi,NULL,NULL,&type_yi,DONT_CARE);

/*
 * Retrieve argument #2
 */
  ti = (void *) NclGetArgValue(
          2,
          9,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_ti[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvd: the length of ti is greater than INT_MAX");
    return(NhlFATAL);
  }
  mpts  = dsizes_ti[0];
  impts = (int) mpts;

/*
 * Retrieve argument #3
 */
  xo = (void *) NclGetArgValue(
          3,
          9,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Retrieve argument #4
 */
  yo = (void *) NclGetArgValue(
          4,
          9,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

/*
 * Retrieve argument #5 - xd
 */
  xd = (void *) NclGetArgValue(
          5,
          9,
          NULL,
          dsizes_xd,
          NULL,
          NULL,
          &type_xd,
          DONT_CARE);

/*
 * Retrieve argument #6 - yd
 */
  yd = (void *) NclGetArgValue(
          6,
          9,
          NULL,
          dsizes_yd,
          NULL,
          NULL,
          &type_yd,
          DONT_CARE);

/*
 * Retrieve argument #8 - xdd
 */
  xdd = (void *) NclGetArgValue(
          7,
          9,
          NULL,
          dsizes_xdd,
          NULL,
          NULL,
          &type_xdd,
          DONT_CARE);

/*
 * Retrieve argument #9 - ydd
 */
  ydd = (void *) NclGetArgValue(
          8,
          9,
          NULL,
          dsizes_ydd,
          NULL,
          NULL,
          &type_ydd,
          DONT_CARE);

/*
 * Coerce input and create space for output if necessary. If
 * any of the input is double, then promote everything to double
 * and call the double precision version of the routine.
 * Otherwise, promote everything to float and call the float routine.
 */
  if(type_xi == NCL_double || type_yi == NCL_double ||
     type_ti == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double || type_xd == NCL_double ||
     type_yd == NCL_double || type_xdd == NCL_double ||
     type_ydd == NCL_double) {

    type_tmp = NCL_double;

    tmp_xi = coerce_input_double(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_double(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_double(ti, type_ti, mpts, 0, NULL, NULL);

    if(type_xo == NCL_float) {
      tmp_xo = (double*) calloc(mpts, sizeof(double));
      if( tmp_xo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for xo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xo = &((double*)xo) [0];
    }
    if(type_yo == NCL_float) {
      tmp_yo = (double*) calloc(mpts, sizeof(double));
      if( tmp_yo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for yo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yo = &((double*)yo) [0];
    }
    if(type_xd == NCL_float) {
      tmp_xd = (double*) calloc(mpts, sizeof(double));
      if( tmp_xd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for xd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xd = &((double*)xd) [0];
    }
    if(type_yd == NCL_float) {
      tmp_yd = (double*) calloc(mpts, sizeof(double));
      if( tmp_yd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for yd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yd = &((double*)yd) [0];
    }
    if(type_xdd == NCL_float) {
      tmp_xdd = (double*) calloc(mpts, sizeof(double));
      if( tmp_xdd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for xdd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xdd = &((double*)xdd) [0];
    }
    if(type_ydd == NCL_float) {
      tmp_ydd = (double*) calloc(mpts, sizeof(double));
      if( tmp_ydd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to allocate memory for ydd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_ydd = &((double*)ydd) [0];
    }
  }
  else {
    type_tmp = NCL_float;

    tmp_xi = coerce_input_float(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_float(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_float(ti, type_ti, mpts, 0, NULL, NULL);
    tmp_xo = &((float*)xo) [0];
    tmp_yo = &((float*)yo) [0];
    tmp_xd = &((float*)xd) [0];
    tmp_yd = &((float*)yd) [0];
    tmp_xdd = &((float*)xdd) [0];
    tmp_ydd = &((float*)ydd) [0];
  }

  if( tmp_xi == NULL || tmp_yi == NULL || tmp_ti == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvd: Unable to coerce xi or yi or ti to double or float");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */
  if(type_tmp == NCL_float) {
    fterr = c_ftkurvd(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo,
                      tmp_xd,tmp_yd,tmp_xdd,tmp_ydd);
  }
  else {
    fterr = c_ftkurvddp(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo,
                        tmp_xd,tmp_yd,tmp_xdd,tmp_ydd);

    if(type_xo == NCL_float) {
      coerce_output_float_only(xo,tmp_xo,mpts,0);
    }
    if(type_yo == NCL_float) {
      coerce_output_float_only(yo,tmp_yo,mpts,0);
    }
    if(type_xd == NCL_float) {
      coerce_output_float_only(xd,tmp_xd,mpts,0);
    }
    if(type_yd == NCL_float) {
      coerce_output_float_only(yd,tmp_yd,mpts,0);
    }
    if(type_xdd == NCL_float) {
      coerce_output_float_only(xdd,tmp_xdd,mpts,0);
    }
    if(type_ydd == NCL_float) {
      coerce_output_float_only(ydd,tmp_ydd,mpts,0);
    }
  }
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvd: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
/*
 * Free up memory we don't need.
 */
  if(type_tmp == NCL_float) {
    if(type_xi != NCL_float) NclFree(tmp_xi);
    if(type_yi != NCL_float) NclFree(tmp_yi);
    if(type_ti != NCL_float) NclFree(tmp_ti);
  }
  else {
    if(type_xi != NCL_double) NclFree(tmp_xi);
    if(type_yi != NCL_double) NclFree(tmp_yi);
    if(type_ti != NCL_double) NclFree(tmp_ti);
    if(type_xo != NCL_double) NclFree(tmp_xo);
    if(type_yo != NCL_double) NclFree(tmp_yo);
    if(type_xd != NCL_double) NclFree(tmp_xd);
    if(type_yd != NCL_double) NclFree(tmp_yd);
    if(type_xdd!= NCL_double) NclFree(tmp_xdd);
    if(type_ydd!= NCL_double) NclFree(tmp_ydd);
  }

  return(NhlNOERROR);
}

NhlErrorTypes ftkurvpd_W(void)
{

/*
 * Input array variables
 */
  void *xi, *yi, *xo, *ti, *yo, *xd, *yd, *xdd, *ydd;
  ng_size_t dsizes_xi[1], dsizes_yi[1], dsizes_ti[1];
  ng_size_t dsizes_xo[1], dsizes_yo[1];
  ng_size_t dsizes_xd[1], dsizes_yd[1];
  ng_size_t dsizes_xdd[1], dsizes_ydd[1];

  ng_size_t npts, mpts;
  int inpts, impts;
  void *tmp_xi,*tmp_yi,*tmp_xo,*tmp_yo,*tmp_ti;
  void *tmp_xd,*tmp_yd,*tmp_xdd,*tmp_ydd;
  NclBasicDataTypes type_xi,type_yi,type_xo,type_yo,type_ti,type_tmp;
  NclBasicDataTypes type_xd,type_yd,type_xdd,type_ydd;

/*
 * Retrieve argument #1
 */
  xi = (void *) NclGetArgValue(0,9,NULL,dsizes_xi,NULL,NULL,&type_xi,DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_xi[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvpd: the length of xi is greater than INT_MAX");
    return(NhlFATAL);
  }
  npts  = dsizes_xi[0];
  inpts = (int) npts;

/*
 * Retrieve argument #2
 */
  yi = (void *) NclGetArgValue(1,9,NULL,dsizes_yi,NULL,NULL,&type_yi,DONT_CARE);

/*
 * Retrieve argument #3
 */
  ti = (void *) NclGetArgValue(
          2,
          9,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          DONT_CARE);

/*
 * Test the dimension sizes.
 */
  if(dsizes_ti[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftkurvpd: the length of ti is greater than INT_MAX");
    return(NhlFATAL);
  }
  mpts  = dsizes_ti[0];
  impts = (int) mpts;

/*
 * Retrieve argument #4
 */
  xo = (void *) NclGetArgValue(
          3,
          9,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Retrieve argument #5
 */
  yo = (void *) NclGetArgValue(
          4,
          9,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

/*
 * Retrieve argument #6 - xd
 */
  xd = (void *) NclGetArgValue(
          5,
          9,
          NULL,
          dsizes_xd,
          NULL,
          NULL,
          &type_xd,
          DONT_CARE);

/*
 * Retrieve argument #7 - yd
 */
  yd = (void *) NclGetArgValue(
          6,
          9,
          NULL,
          dsizes_yd,
          NULL,
          NULL,
          &type_yd,
          DONT_CARE);

/*
 * Retrieve argument #7 - xdd
 */
  xdd = (void *) NclGetArgValue(
          7,
          9,
          NULL,
          dsizes_xdd,
          NULL,
          NULL,
          &type_xdd,
          DONT_CARE);

/*
 * Retrieve argument #8 - ydd
 */
  ydd = (void *) NclGetArgValue(
          8,
          9,
          NULL,
          dsizes_ydd,
          NULL,
          NULL,
          &type_ydd,
          DONT_CARE);

/*
 * Coerce input and create space for output if necessary. If
 * any of the input is double, then promote everything to double
 * and call the double precision version of the routine.
 * Otherwise, promote everything to float and call the float routine.
 */
  if(type_xi == NCL_double || type_yi == NCL_double ||
     type_ti == NCL_double || type_xo == NCL_double ||
     type_yo == NCL_double || type_xd == NCL_double ||
     type_yd == NCL_double || type_xdd == NCL_double ||
     type_ydd == NCL_double) {

    type_tmp = NCL_double;

    tmp_xi = coerce_input_double(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_double(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_double(ti, type_ti, mpts, 0, NULL, NULL);

    if(type_xo == NCL_float) {
      tmp_xo = (double*) calloc(mpts, sizeof(double));
      if( tmp_xo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for xo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xo = &((double*)xo) [0];
    }
    if(type_yo == NCL_float) {
      tmp_yo = (double*) calloc(mpts, sizeof(double));
      if( tmp_yo == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for yo");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yo = &((double*)yo) [0];
    }
    if(type_xd == NCL_float) {
      tmp_xd = (double*) calloc(mpts, sizeof(double));
      if( tmp_xd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for xd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xd = &((double*)xd) [0];
    }
    if(type_yd == NCL_float) {
      tmp_yd = (double*) calloc(mpts, sizeof(double));
      if( tmp_yd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for yd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_yd = &((double*)yd) [0];
    }
    if(type_xdd == NCL_float) {
      tmp_xdd = (double*) calloc(mpts, sizeof(double));
      if( tmp_xdd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for xdd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_xdd = &((double*)xdd) [0];
    }
    if(type_ydd == NCL_float) {
      tmp_ydd = (double*) calloc(mpts, sizeof(double));
      if( tmp_ydd == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to allocate memory for ydd");
        return(NhlFATAL);
      }
    }
    else {
      tmp_ydd = &((double*)ydd) [0];
    }
  }
  else {
    type_tmp = NCL_float;

    tmp_xi = coerce_input_float(xi, type_xi, npts, 0, NULL, NULL);
    tmp_yi = coerce_input_float(yi, type_yi, npts, 0, NULL, NULL);
    tmp_ti = coerce_input_float(ti, type_ti, mpts, 0, NULL, NULL);
    tmp_xo = &((float*)xo) [0];
    tmp_yo = &((float*)yo) [0];
    tmp_xd = &((float*)xd) [0];
    tmp_yd = &((float*)yd) [0];
    tmp_xdd = &((float*)xdd) [0];
    tmp_ydd = &((float*)ydd) [0];
  }

  if( tmp_xi == NULL || tmp_yi == NULL || tmp_ti == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftkurvpd: Unable to coerce xi or yi or ti to double or float");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */
  if(type_tmp == NCL_float) {
    fterr = c_ftkurvpd(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo,
                       tmp_xd,tmp_yd,tmp_xdd,tmp_ydd);
  }
  else {
    fterr = c_ftkurvpddp(inpts,tmp_xi,tmp_yi,impts,tmp_ti,tmp_xo,tmp_yo,
                         tmp_xd,tmp_yd,tmp_xdd,tmp_ydd);

    if(type_xo == NCL_float) {
      coerce_output_float_only(xo,tmp_xo,mpts,0);
    }
    if(type_yo == NCL_float) {
      coerce_output_float_only(yo,tmp_yo,mpts,0);
    }
    if(type_xd == NCL_float) {
      coerce_output_float_only(xd,tmp_xd,mpts,0);
    }
    if(type_yd == NCL_float) {
      coerce_output_float_only(yd,tmp_yd,mpts,0);
    }
    if(type_xdd == NCL_float) {
      coerce_output_float_only(xdd,tmp_xdd,mpts,0);
    }
    if(type_ydd == NCL_float) {
      coerce_output_float_only(ydd,tmp_ydd,mpts,0);
    }
  }
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvpd: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }

/*
 * Free up memory we don't need.
 */
  if(type_tmp == NCL_float) {
    if(type_xi != NCL_float) NclFree(tmp_xi);
    if(type_yi != NCL_float) NclFree(tmp_yi);
    if(type_ti != NCL_float) NclFree(tmp_ti);
  }
  else {
    if(type_xi != NCL_double) NclFree(tmp_xi);
    if(type_yi != NCL_double) NclFree(tmp_yi);
    if(type_ti != NCL_double) NclFree(tmp_ti);
    if(type_xo != NCL_double) NclFree(tmp_xo);
    if(type_yo != NCL_double) NclFree(tmp_yo);
    if(type_xd != NCL_double) NclFree(tmp_xd);
    if(type_yd != NCL_double) NclFree(tmp_yd);
    if(type_xdd!= NCL_double) NclFree(tmp_xdd);
    if(type_ydd!= NCL_double) NclFree(tmp_ydd);
  }

  return(NhlNOERROR);
}

NhlErrorTypes ftsurf_W(void)
{
/*
 * Input array variables
 */
  void *xi, *yi, *zi, *xo, *yo, *zo;
  int ndims_xi;
  ng_size_t dsizes_xi[NCL_MAX_DIMENSIONS];
  int ndims_yi;
  ng_size_t dsizes_yi[NCL_MAX_DIMENSIONS];
  int ndims_zi;
  ng_size_t dsizes_zi[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_xo[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_yo[NCL_MAX_DIMENSIONS];
  int ndims_zo;
  ng_size_t dsizes_zo[NCL_MAX_DIMENSIONS];

  ng_size_t i,index_xi,index_yi,index_zi,index_zo;
  ng_size_t k,nxi,nyi,nxinyi,nxo,nyo,nxonyo,nt;
  int inxi,inyi,inxo,inyo;
  void *tmp_xi, *tmp_yi, *tmp_xo, *tmp_yo, *tmp_zi;
  NclBasicDataTypes type_xi, type_yi, type_xo, type_yo, type_zi, type_zo;
  float *ztmp_ft = NULL;
  double *ztmp_dp = NULL;

/*
 * Retrieve argument #0
 */
  xi = (void *) NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          DONT_CARE);
/*
 * Retrieve argument #1
 */
  yi = (void *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          DONT_CARE);
/*
 * Retrieve argument #2
 */
  zi = (void *) NclGetArgValue(
          2,
          5,
          &ndims_zi,
          dsizes_zi,
          NULL,
          NULL,
          &type_zi,
          DONT_CARE);

/*
 * Check number of dimensions for argument #2.  This argument must
 * have at least two dimensions, but can have more.
 *
 */
  if(ndims_zi < 2) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: zi must have at least two dimensions.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3
 */
  xo = (void *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          DONT_CARE);

/*
 * Retrieve argument #4
 */
  yo = (void *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          DONT_CARE);

/*
 * Test and save the sizes of the last two dimensions of the output array.
 */
  if( (dsizes_xi[ndims_xi-1] > INT_MAX) ||
      (dsizes_yi[ndims_yi-1] > INT_MAX) ||
      (dsizes_xo[0] > INT_MAX) ||
      (dsizes_yo[0] > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"ftsurf: one or more dimension sizes are greater than INT_MAX");
    return(NhlFATAL);
  }
  nxi  = dsizes_xi[ndims_xi-1];
  nyi  = dsizes_yi[ndims_yi-1];
  nxo  = dsizes_xo[0];
  nyo  = dsizes_yo[0];
  inxi = (int) nxi;
  inyi = (int) nyi;
  inxo = (int) nxo;
  inyo = (int) nyo;

  nxinyi = nxi * nyi;
  nxonyo = nxo * nyo;

/*
 * Check dimensions of xi, yi, and zi.
 */
  if(ndims_xi > 1) {
    if(ndims_xi != ndims_zi-1) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftsurf: If xi is not 1-dimensional, then its leftmost-1 dimensions must be the same size as the leftmost-1 dimensions of zi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_xi-1; i++) {
      if(dsizes_xi[i] != dsizes_zi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftsurf: If xi is not 1-dimensional, then its leftmost-1 dimensions must be the same size as the leftmost-1 dimensions of zi");
        return(NhlFATAL);
      }
    }
  }
  if(ndims_yi > 1) {
    if(ndims_yi != ndims_zi-1) {
      NhlPError(NhlFATAL, NhlEUNKNOWN,
                "ftsurf: If yi is not 1-dimensional, then its leftmost-1 dimensions must be the same size as the leftmost-1 dimensions of zi");
      return(NhlFATAL);
    }
    for(i = 0; i < ndims_yi-1; i++) {
      if(dsizes_yi[i] != dsizes_zi[i]) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                  "ftsurf: If yi is not 1-dimensional, then its leftmost-1 dimensions must be the same size as the leftmost-1 dimensions of zi");
        return(NhlFATAL);
      }
    }
  }
/*
 * Check the rightmost two dimensions of zi.
 */
  if( (dsizes_zi[ndims_zi-1] != nyi) || (dsizes_zi[ndims_zi-2] != nxi) ) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: The rightmost two dimensions of zi must be the same as the rightmost dimensions of xi and yi, respectively");
    return(NhlFATAL);
  }

/*
 * Compute the total number of interpolations in the input array.
 */
  nt = 1;
  for(i = 0; i < ndims_zi-2; i++) {
    nt *= dsizes_zi[i];
  }

/*
 * Coercing inputs and allocating memory for output arrays.
a */
  if(type_xi != NCL_double && type_yi != NCL_double && 
     type_xo != NCL_double && type_yo != NCL_double &&
     type_zi != NCL_double) {
    type_zo = NCL_float;
    tmp_xi = (float*) calloc(nxi, sizeof(float));
    tmp_yi = (float*) calloc(nyi, sizeof(float));
    tmp_zi = (float *) calloc(nxinyi, sizeof(float));
    tmp_xo = coerce_input_float(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_yo = coerce_input_float(yo, type_yo, nyo, 0, NULL, NULL);
    zo = (float *) calloc(nt*nxonyo, sizeof(float));
  }
  else {
    type_zo = NCL_double;
    tmp_xi = (double*) calloc(nxi, sizeof(double));
    tmp_yi = (double*) calloc(nyi, sizeof(double));
    tmp_zi = (double *) calloc(nxinyi, sizeof(double));
    tmp_xo = coerce_input_double(xo, type_xo, nxo, 0, NULL, NULL);
    tmp_yo = coerce_input_double(yo, type_yo, nyo, 0, NULL, NULL);
    zo = (double *) calloc(nt*nxonyo, sizeof(double));
  }

  if( tmp_zi == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftsurf: Unable to allocate memory for coercing zi array to float or double");
    return(NhlFATAL);
  }
  if( tmp_xi == NULL || tmp_yi == NULL || tmp_xo == NULL || tmp_yo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN, "ftsurf: Unable to coerce xi or yi or xo or yo to double or float");
    return(NhlFATAL);
  }
  if( zo == NULL ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
            "ftsurf: Unable to allocate memory for output array");
    return(NhlFATAL);
  }

/*
 * Set up dimension sizes for output array.
 */
    ndims_zo = ndims_zi;
    for(i = 0; i < ndims_zi-2; i++) {
      dsizes_zo[i] = dsizes_zi[i];
    }
    dsizes_zo[ndims_zi-2] = nxo;
    dsizes_zo[ndims_zi-1] = nyo;
/*
 * Loop through leftmost points and call C version of routine.
 */
    index_xi = index_yi = index_zi = index_zo = 0;
    for (i = 0; i < nt; i++) {
/*
 * Coerce xi only once if it is one-dimensional.
 */
      if(ndims_xi > 1 || !i) {
        if(type_zo == NCL_float ) {
          coerce_subset_input_float(xi,tmp_xi,index_xi,type_xi,nxi,
                                    0,NULL,NULL);
        }
        else {
          coerce_subset_input_double(xi,tmp_xi,index_xi,type_xi,nxi,
                                     0,NULL,NULL);
        }
      }
/*
 * Coerce yi only once if it is one-dimensional.
 */
      if(ndims_yi > 1 || !i) {
        if(type_zo == NCL_float ) {
          coerce_subset_input_float(yi,tmp_yi,index_yi,type_yi,nyi,
                                    0,NULL,NULL);
        }
        else {
          coerce_subset_input_double(yi,tmp_yi,index_yi,type_yi,nyi,
                                     0,NULL,NULL);
        }
      }
      if(type_zo == NCL_float ) {
        coerce_subset_input_float(zi,tmp_zi,index_zi,type_zi,nxinyi,
                                  0,NULL,NULL);
      }
      else {
        coerce_subset_input_double(zi,tmp_zi,index_zi,type_zi,nxinyi,
                                   0,NULL,NULL);
      }

      fterr = 0;
      if(type_zo == NCL_float) {
        ztmp_ft = c_ftsurf(inxi,inyi,tmp_xi,tmp_yi,tmp_zi,inxo,inyo,tmp_xo,
                           tmp_yo,&fterr);
      }
      else {
        ztmp_dp = c_ftsurfdp(inxi,inyi,tmp_xi,tmp_yi,tmp_zi,inxo,inyo,tmp_xo,
                             tmp_yo,&fterr);
      }
      if (fterr != 0) {
        sprintf(ftmsg, "ftsurf: Error number %d.", fterr);
        NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
        return(NhlFATAL);
      }
      if(type_zo == NCL_float ) {
        for (k = 0; k < nxonyo; k++) {
          ((float*)zo)[index_zo+k] = ztmp_ft[k];
        }
        free(ztmp_ft);
      }
      else {
        for (k = 0; k < nxonyo; k++) {
          ((double*)zo)[index_zo+k] = ztmp_dp[k];
        }
        free(ztmp_dp);
      }
      if(ndims_xi > 1) index_xi += nxi;
      if(ndims_yi > 1) index_yi += nyi;
      index_zi += nxinyi;
      index_zo += nxonyo;
    }

    if(type_xo != type_zo) NclFree(tmp_xo);
    if(type_yo != type_zo) NclFree(tmp_yo);
    NclFree(tmp_xi);
    NclFree(tmp_yi);
    NclFree(tmp_zi);

    return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,type_zo,0));
}

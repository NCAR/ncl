#include <stdio.h>
#include <string.h>
#include "wrapper.h"
#include <ncarg/ngmath.h>

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
  char *params_f[] = {"sig", "sl1", "sl2", "smt", "eps", 
                      "SIG", "SL1", "SL2", "SMT", "EPS"};
  char *params_c[] = {"dum", "DUM"};

/*
 * Input array variables
 */
  string *pname;
  int dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname, type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (string *) NclGetArgValue(
          0,
          2,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          2);

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
           2);

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
        cval = NrmQuarkToString( *((string *) pvalue));
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
  string *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"sf1", "sf2", "SF1", "SF2"};
  char *params_f[] = {"sig", "sl1", "sl2", "smt", "eps", 
                      "SIG", "SL1", "SL2", "SMT", "EPS"};
  char *params_c[] = {"dum", "DUM"};

/*
 * Input array variable
 */
  string *pname;
  int dsizes_pname[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname;
  float *fval;
  int *ival;
  int ret_size = 1;

/*
 * Retrieve argument #1
 */
  pname = (string *) NclGetArgValue(
          0,
          1,
          NULL,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          2);

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
      qvalue = (string *) calloc(1,sizeof(string));
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
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  float *yo;
  int *dsizes_yo;
/*
 * Various
 */
  int i, npts, nxo, size_leftmost, index_in = 0, index_out = 0;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          3,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          3,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #1.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurv: The last dimension of argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          2,
          3,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

  nxo = dsizes_xo[0];

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 *  Acquire space for the return value.
 */
  yo        = (float *) calloc(size_leftmost*nxo, sizeof(float));
  dsizes_yo =   (int *) calloc(   ndims_yi, sizeof(int));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurv: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurv(npts, xi, &yi[index_in], nxo, xo, &yo[index_out]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurv: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    index_in  += npts;
    index_out += nxo;
  }

  return(NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,NCL_float,0 ));
}

NhlErrorTypes ftcurvd_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  float *yo;
  int *dsizes_yo;
/*
 * Various
 */
  int i, npts, nxo, size_leftmost, index_xi = 0, index_yi = 0, index_out = 0;

/*
 * Retrieve xi.
 */
  xi = (float *) NclGetArgValue(
          0,
          3,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[ndims_xi-1];

/*
 * Retrieve yi.
 */
  yi = (float *) NclGetArgValue(
          1,
          3,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of yi.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvd: The last dimension of yi must be the same length as xi");
    return(NhlFATAL);
  }

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
          NULL,
          2);

  nxo = dsizes_xo[0];

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 *  Acquire space for the return value.
 */
  yo        = (float *) calloc(size_leftmost*nxo, sizeof(float));
  dsizes_yo =   (int *) calloc(   ndims_yi, sizeof(int));

  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvd: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvd(npts, &xi[index_xi], &yi[index_yi], nxo, xo, &yo[index_out]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvd: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    if(ndims_xi > 1) index_xi += npts;
    index_yi  += npts;
    index_out += nxo;
  }
  return(NclReturnValue((void *)yo,ndims_yi,dsizes_yo,NULL,NCL_float,0 ));
}

NhlErrorTypes ftcurvi_W(void)
{

/*
 * Input variables.
 */
  float *xl;
  float *xr;
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  float *integral;
  int *dsizes_int, ndims_int;

/*
 * Various
 */
  int i, npts, size_leftmost, index_in = 0;

/*
 * Retrieve argument #1
 */
  xl = (float *) NclGetArgValue(
          0,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #2
 */
  xr = (float *) NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3
 */
  xi = (float *) NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #4
 */
  yi = (float *) NclGetArgValue(
          3,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #4.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvi: The last dimension of argument #4 must be the same length as argument #3");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 *  Acquire space for the return value.
 */
  ndims_int  = max(1,ndims_yi-1);
  integral   = (float *) calloc(size_leftmost,sizeof(float));
  dsizes_int = (int *) calloc(ndims_int,sizeof(int));

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
 *  Calculate the integral values.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvi(*xl, *xr, npts, xi, &yi[index_in], &integral[i]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvi: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    index_in += npts;
  }

  return(NclReturnValue((void *) integral, ndims_int, dsizes_int, NULL,
                        NCL_float, 0));
}

NhlErrorTypes ftcurvp_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *p;
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  float *yo;
  int *dsizes_yo;

/*
 * Various
 */
  int i, npts, nxo, size_leftmost, index_in = 0, index_out = 0;

/*
 * Retrieve argument #1 (X coordinate input values)
 */
  xi = (float *) NclGetArgValue(
          0,
          4,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #2 (Y coordinate input values)
 */
  yi = (float *) NclGetArgValue(
          1,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #4.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvp: The last dimension of argument #4 must be the same length as argument #3");
    return(NhlFATAL);
  }


/*
 * Retrieve argument #3 (The period)
 */
  p = (float *) NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

  nxo = dsizes_xo[0];

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 *  Acquire space for the return value.
 */
  yo        = (float *) calloc(size_leftmost*nxo, sizeof(float)); 
  dsizes_yo =   (int *) calloc(   ndims_yi, sizeof(int));
  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvp: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Call the C procedure.
 */

  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvp(npts, xi, &yi[index_in], *p, nxo, xo, &yo[index_out]);

    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvp: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }

    index_in  += npts;
    index_out += nxo;
  }
  return(NclReturnValue((void *) yo, ndims_yi, dsizes_yo, NULL, 
                        NCL_float, 0));
}

NhlErrorTypes ftcurvpi_W(void)
{

/*
 * Input variables.
 */
  float *xl;
  float *xr;
  float *p;
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];

/*
 * Output variables.
 */
  float *integral;
  int *dsizes_int, ndims_int;

/*
 * Various
 */
  int i, npts, size_leftmost, index_in = 0;

/*
 * Retrieve argument #1 (left integral limit).
 */
  xl = (float *) NclGetArgValue(
          0,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #2 (right integral limit).
 */
  xr = (float *) NclGetArgValue(
          1,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3 (The period)
 */
  p = (float *) NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4 (the X coordinate input values).
 */
  xi = (float *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #5 (the Y coordinate input values).
 */
  yi = (float *) NclGetArgValue(
          4,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #4.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: The last dimension of argument #4 must be the same length as argument #3");
    return(NhlFATAL);
  }

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 *  Acquire space for the return value.
 */
  ndims_int  = max(1,ndims_yi-1);
  integral   = (float *) calloc(size_leftmost,sizeof(float));
  dsizes_int = (int *) calloc(ndims_int,sizeof(int));

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
 *  Return the integral value.
 */
  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvpi(*xl, *xr, *p, npts, xi, &yi[index_in], &integral[i]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvpi: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    index_in += npts;
  }
  return(NclReturnValue((void *) integral, ndims_int, dsizes_int, NULL,
                        NCL_float, 0));
}

NhlErrorTypes ftcurvs_W(void)
{

/*
 * Input variables.
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *d;
  int dsizes_d[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
/*
 * Output variables.
 */
  float *yo;
  int *dsizes_yo;

/*
 * Various
 */
  int i, npts, nxo, size_leftmost, index_in = 0, index_out = 0;
  int isw;

/*
 * Retrieve argument #1 (X coordinate input points).
 */
  xi = (float *) NclGetArgValue(
          0,
          4,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #2 (Y coordinate input values).
 */
  yi = (float *) NclGetArgValue(
          1,
          4,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #1.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: The last dimension of argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (The observation weights).
 */
  d = (float *) NclGetArgValue(
          2,
          4,
          NULL,
          dsizes_d,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3 (the X coordinate output values).
 */
  xo = (float *) NclGetArgValue(
          3,
          4,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

  nxo = dsizes_xo[0];

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Allocate space for output variables.
 */
  yo        = (float *) calloc(size_leftmost*nxo, sizeof(float)); 
  dsizes_yo =   (int *) calloc(   ndims_yi, sizeof(int));
  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvs: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Return the interpolated smooth curve.
 */
  if (dsizes_d[0] > 1) isw = 0;
  isw = 1;
  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvs(npts, xi, &yi[index_in], isw, d, nxo, xo, 
                      &yo[index_out]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvs: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    index_in  += npts;
    index_out += nxo;
  }
  return(NclReturnValue((void *) yo, ndims_yi, dsizes_yo, NULL, 
                        NCL_float, 0));
}

NhlErrorTypes ftcurvps_W(void)
{

/*
 * Input variables.
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *p;
  float *d;
  int dsizes_d[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];

/*
 * Output variables.
 */
  float *yo;
  int *dsizes_yo;

/*
 * Various
 */
  int i, npts, nxo, size_leftmost, index_in = 0, index_out = 0;
  int isw;

/*
 * Retrieve argument #0 (X coordinate input points).
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #1 (Y coordinate input values).
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check last dimension of argument #1.
 */
  if(dsizes_yi[ndims_yi-1] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: The last dimension of argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #2 (The period).
 */
  p = (float *) NclGetArgValue(
          2,
          5,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3 (The observation weights).
 */
  d = (float *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_d,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4 (the X coordinate output values).
 */
  xo = (float *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

  nxo = dsizes_xo[0];

/*
 * Compute the total size of the leftmost dimension.
 */
  size_leftmost = 1;
  for( i = 0; i < ndims_yi-1; i++ ) size_leftmost *= dsizes_yi[i];

/*
 * Allocate space for output variables.
 */
  yo        = (float *) calloc(size_leftmost*nxo, sizeof(float)); 
  dsizes_yo =   (int *) calloc(   ndims_yi, sizeof(int));
  if(yo == NULL || dsizes_yo == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
              "ftcurvps: Unable to allocate memory for output arrays");
    return(NhlFATAL);
  }

  for( i = 0; i < ndims_yi-1; i++ ) dsizes_yo[i] = dsizes_yi[i];
  dsizes_yo[ndims_yi-1] = nxo;

/*
 *  Return the interpolated smooth curve.
 */
  isw = 1;
  if (dsizes_d[0] > 1) isw = 0;
  for( i = 0; i < size_leftmost; i++ ) {
    fterr = c_ftcurvps(npts, xi, &yi[index_in], *p, isw, d, nxo, xo,
                       &yo[index_out]);
    if (fterr != 0) {
      sprintf(ftmsg, "ftcurvps: Error number %d.", fterr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
      return(NhlFATAL);
    }
    index_in  += npts;
    index_out += nxo;
  }
  return(NclReturnValue((void *) yo, ndims_yi, dsizes_yo, NULL, 
                        NCL_float, 0));
}

NhlErrorTypes ftkurv_W(void)
{
/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  int npts, mpts;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check dimension of argument #4.
 */
  if(dsizes_yi[0] != npts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #1 must be the same length as argument #0");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          NULL,
          2);

  mpts = dsizes_ti[0];

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check dimension of arguments #2, 3, and #4.
 */
  if(dsizes_xo[0] != mpts || dsizes_yo[0] != mpts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Arguments #2, 3, and #4 must be the same length");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurv(npts, xi, yi, mpts, ti, xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurv: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes ftkurvp_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];

  int npts, mpts;

/*
 * Retrieve argument #0
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          NULL,
          dsizes_xi,
          NULL,
          NULL,
          NULL,
          2);

  npts = dsizes_xi[0];

/*
 * Retrieve argument #1
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          NULL,
          dsizes_yi,
          NULL,
          NULL,
          NULL,
          2);

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
  ti = (float *) NclGetArgValue(
          2,
          5,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          NULL,
          2);

  mpts = dsizes_ti[0];

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Check dimension of arguments #2, 3, and #4.
 */
  if(dsizes_xo[0] != mpts || dsizes_yo[0] != mpts) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Arguments #2, 3, and #4 must be the same length");
    return(NhlFATAL);
  }

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurvp(npts, xi, yi, mpts, ti, xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvp: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes ftkurvd_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *xd;
  int dsizes_xd[NCL_MAX_DIMENSIONS];
  float *yd;
  int dsizes_yd[NCL_MAX_DIMENSIONS];
  float *xdd;
  int dsizes_xdd[NCL_MAX_DIMENSIONS];
  float *ydd;
  int dsizes_ydd[NCL_MAX_DIMENSIONS];

/*
 * Retrieve argument #0
 */
  xi = (float *) NclGetArgValue(0,9,NULL,dsizes_xi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #1
 */
  yi = (float *) NclGetArgValue(1,9,NULL,dsizes_yi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #2
 */
  ti = (float *) NclGetArgValue(
          2,
          9,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          3,
          9,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  yo = (float *) NclGetArgValue(
          4,
          9,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #5 - xd
 */
  xd = (float *) NclGetArgValue(
          5,
          9,
          NULL,
          dsizes_xd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #6 - yd
 */
  yd = (float *) NclGetArgValue(
          6,
          9,
          NULL,
          dsizes_yd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #8 - xdd
 */
  xdd = (float *) NclGetArgValue(
          7,
          9,
          NULL,
          dsizes_xdd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #9 - ydd
 */
  ydd = (float *) NclGetArgValue(
          8,
          9,
          NULL,
          dsizes_ydd,
          NULL,
          NULL,
          NULL,
          2);

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurvd(dsizes_xi[0], xi, yi, dsizes_xo[0], ti, xo, yo,
                    xd, yd, xdd, ydd);
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvd: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes ftkurvpd_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *xd;
  int dsizes_xd[NCL_MAX_DIMENSIONS];
  float *yd;
  int dsizes_yd[NCL_MAX_DIMENSIONS];
  float *xdd;
  int dsizes_xdd[NCL_MAX_DIMENSIONS];
  float *ydd;
  int dsizes_ydd[NCL_MAX_DIMENSIONS];

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(0,9,NULL,dsizes_xi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(1,9,NULL,dsizes_yi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          9,
          NULL,
          dsizes_ti,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          9,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          9,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #6 - xd
 */
  xd = (float *) NclGetArgValue(
          5,
          9,
          NULL,
          dsizes_xd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #7 - yd
 */
  yd = (float *) NclGetArgValue(
          6,
          9,
          NULL,
          dsizes_yd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #7 - xdd
 */
  xdd = (float *) NclGetArgValue(
          7,
          9,
          NULL,
          dsizes_xdd,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #8 - ydd
 */
  ydd = (float *) NclGetArgValue(
          8,
          9,
          NULL,
          dsizes_ydd,
          NULL,
          NULL,
          NULL,
          2);

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurvpd(dsizes_xi[0], xi, yi, dsizes_xo[0], ti, xo, yo,
                    xd, yd, xdd, ydd);
  if (fterr != 0) {
    sprintf(ftmsg, "ftkurvpd: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NhlNOERROR);
}

NhlErrorTypes ftsurf_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *xo;
  int dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];

  int i,ji,jo,k,nxi,nyi,nxo,nyo,nt;
  float *ztmp;

/*
 * Retrieve argument #0
 */
  xi = (float *) NclGetArgValue(0,5,NULL,dsizes_xi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #1
 */
  yi = (float *) NclGetArgValue(1,5,NULL,dsizes_yi,NULL,NULL,NULL,2);

/*
 * Retrieve argument #2
 */
  zi = (float *) NclGetArgValue(2,5,&ndims_zi,dsizes_zi,NULL,NULL,NULL,2);

/*
 * Check number of dimensions for argument #2.  This argument must
 * have at least two dimensions, but can have more.
 *
 */
  if(ndims_zi < 2) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #2 has less than two dimensions.");
    return(NhlFATAL);
  }

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          NULL,
          dsizes_xo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Retrieve argument #4
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          NULL,
          dsizes_yo,
          NULL,
          NULL,
          NULL,
          2);

/*
 * Save the sizes of the last two dimensions of the output array.
 */
  nxi = dsizes_xi[0];
  nyi = dsizes_yi[0];
  nxo = dsizes_xo[0];
  nyo = dsizes_yo[0];
/*
 * Compute the total number of interpolations in the input array.
 */
  nt = 1;
  for(i = 0; i < ndims_zi-2; i++) {
    nt *= dsizes_zi[i];
  }

/*
 * Returns value to NCL
 */
    ndims_zo = ndims_zi;
    for(i = 0; i < ndims_zi-2; i++) {
      dsizes_zo[i] = dsizes_zi[i];
    }
    zo =  (float *) calloc(nt*nxo*nyo,sizeof(float));
    dsizes_zo[ndims_zi-2] = dsizes_xo[0];
    dsizes_zo[ndims_zi-1] = dsizes_yo[0];
    ji = 0;
    jo = 0;
    for (i = 0; i < nt; i++) {
      fterr = 0;
      ztmp = c_ftsurf(nxi,nyi,xi,yi,&(zi[ji]),nxo,nyo,xo,yo,&fterr);
      if (fterr != 0) {
        sprintf(ftmsg, "ftsurf: Error number %d.", fterr);
        NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
        return(NhlFATAL);
      }
      for (k = 0; k < nxo*nyo; k++) {
        zo[jo+k] = ztmp[k];
      }
      free(ztmp);
      ji += nxi*nyi;
      jo += nxo*nyo;
    }
    return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_float,0));
}

#include <stdio.h>
#include <string.h>

/*
 * The following are required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include.
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>

/*
 *  Declare the required C functions.
 */
extern int c_ftsetr(char *, float);
extern int c_ftseti(char *, int);
extern int c_ftsetc(char *, char *);
extern int c_ftgetr(char *, float *);
extern int c_ftgeti(char *, int *);
extern int c_ftgetc(char *, char *);
extern int c_ftsetfa(char *, int, float *);
extern int c_ftgetfa_size(char *);
extern float *c_ftgetfa_data(char *);
extern int c_ftcurv(int, float [], float [], int, float [], float []);
extern int c_ftcurvd(int, float [], float [], int, float [], float []);
extern int c_ftcurvi(float, float, int, float [], float [], float *);
extern int c_ftcurvp(int, float [], float [], float, int, float [], float yo[]);
extern int c_ftcurvpi(float, float, float, int, float [], float [], float *);
extern int c_ftcurvs(int, float [], float [], int, float [], int, 
                     float [], float []);
extern int c_ftcurvps(int, float [], float [], float, int, float [],
                      int, float [], float []);
extern int c_ftkurv(int, float [], float [], int, float [], float [], float []);
extern int c_ftkurvd(int, float [], float [], int, float [], float [], 
                     float [], float [], float [], float [], float []);
extern int c_ftkurvp(int, float [], float [], int, float [], float [], 
                     float []);
extern float *c_ftsurf(int, int, float *, float *, float *,
                     int, int, float *, float *, int *);

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
  int ndims_pname, dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int ndims_pvalue, dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname, type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (string *) NclGetArgValue(
          0,
          2,
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
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
           &ndims_pvalue,
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
  string *pvalue, *qvalue;

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
  int ndims_pname, dsizes_pname[NCL_MAX_DIMENSIONS];
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
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          2);


/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftgetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
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
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  NclBasicDataTypes type_xi, type_yi, type_xo;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          3,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurv: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurv: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurv: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurv: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          2,
          3,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurv: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurv: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Acquire space for the return value.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float)); 
  fterr = c_ftcurv(dsizes_xi[0], xi, yi, dsizes_xo[0], xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurv: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) yo, 
                         1,
                         dsizes_xo,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
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
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  NclBasicDataTypes type_xi, type_yi, type_xo;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          3,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvd: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvd: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvd: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvd: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  xo = (float *) NclGetArgValue(
          2,
          3,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvd: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvd: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Acquire space for the return value.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float)); 
  fterr = c_ftcurvd(dsizes_xi[0], xi, yi, dsizes_xo[0], xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvd: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) yo, 
                         1,
                         dsizes_xo,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftcurvi_W(void)
{

/*
 * Input variables.
 */
  float *xl;
  int ndims_xl, dsizes_xl[NCL_MAX_DIMENSIONS];
  float *xr;
  int ndims_xr, dsizes_xr[NCL_MAX_DIMENSIONS];
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *integral;
  NclBasicDataTypes type_xl, type_xr, type_xi, type_yi;
  int ret_size = 1;

/*
 * Retrieve argument #1
 */
  xl = (float *) NclGetArgValue(
          0,
          4,
          &ndims_xl,
          dsizes_xl,
          NULL,
          NULL,
          &type_xl,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xl != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvi: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xl != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvi: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  xr = (float *) NclGetArgValue(
          1,
          4,
          &ndims_xr,
          dsizes_xr,
          NULL,
          NULL,
          &type_xr,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_xr != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvi: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_xr != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvi: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }
/*
 * Retrieve argument #3
 */
  xi = (float *) NclGetArgValue(
          2,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvi: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvi: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvi: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvi: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Return the integral value.
 */
  integral = (float *) calloc(1,sizeof(float));
  fterr = c_ftcurvi(*xl, *xr, dsizes_xi[0], xi, yi, integral);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvi: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) integral, 
                         1,
                         &ret_size,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftcurvp_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *p;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  NclBasicDataTypes type_xi, type_yi, type_p, type_xo;

/*
 * Retrieve argument #1 (X coordinate input values)
 */
  xi = (float *) NclGetArgValue(
          0,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvp: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvp: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvp: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3 (The period)
 */
  p = (float *) NclGetArgValue(
          2,
          4,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_p != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvp: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_p != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvp: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          4,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvp: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvp: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Acquire space for the return value.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float)); 
  fterr = c_ftcurvp(dsizes_xi[0], xi, yi, *p, dsizes_xo[0], xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvp: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) yo, 
                         1,
                         dsizes_xo,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftcurvpi_W(void)
{

/*
 * Input variables.
 */
  float *xl;
  int ndims_xl, dsizes_xl[NCL_MAX_DIMENSIONS];
  float *xr;
  int ndims_xr, dsizes_xr[NCL_MAX_DIMENSIONS];
  float *p;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *integral;
  NclBasicDataTypes type_xl, type_xr, type_xi, type_yi, type_p;
  int ret_size = 1;	

/*
 * Retrieve argument #1 (left integral limit).
 */
  xl = (float *) NclGetArgValue(
          0,
          5,
          &ndims_xl,
          dsizes_xl,
          NULL,
          NULL,
          &type_xl,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xl != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xl != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvpi: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2 (right integral limit).
 */
  xr = (float *) NclGetArgValue(
          1,
          5,
          &ndims_xr,
          dsizes_xr,
          NULL,
          NULL,
          &type_xr,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_xr != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_xr != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvpi: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3 (The period)
 */
  p = (float *) NclGetArgValue(
          2,
          5,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_p != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_p != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvpi: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4 (the X coordinate input values).
 */
  xi = (float *) NclGetArgValue(
          3,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvpi: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvpi: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvpi: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Return the integral value.
 */
  integral = (float *) calloc(1,sizeof(float));
  fterr = c_ftcurvpi(*xl, *xr, *p, dsizes_xi[0], xi, yi, integral);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvpi: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) integral, 
                         1,
                         &ret_size,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftcurvs_W(void)
{

/*
 * Input variables.
 */
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *d;
  int ndims_d, dsizes_d[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int isw;
  NclBasicDataTypes type_xi, type_yi, type_d, type_xo;

/*
 * Retrieve argument #1 (X coordinate input points).
 */
  xi = (float *) NclGetArgValue(
          0,
          4,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvs: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

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
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvs: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3 (The observation weights).
 */
  d = (float *) NclGetArgValue(
          2,
          4,
          &ndims_d,
          dsizes_d,
          NULL,
          NULL,
          &type_d,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_d != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_d != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvs: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4 (the X coordinate output values).
 */
  xo = (float *) NclGetArgValue(
          3,
          4,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvs: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Return the interpolated smooth curve.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float)); 
  isw = 1;
  if (dsizes_d[0] > 1) isw = 0;
  fterr = c_ftcurvs(dsizes_xi[0], xi, yi, isw, d, dsizes_xo[0], xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvs: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) yo, 
                         1,
                         dsizes_xo,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftcurvps_W(void)
{

/*
 * Input variables.
 */
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *p;
  int ndims_p, dsizes_p[NCL_MAX_DIMENSIONS];
  float *d;
  int ndims_d, dsizes_d[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int isw;
  NclBasicDataTypes type_xi, type_yi, type_p, type_d, type_xo;

/*
 * Retrieve argument #1 (X coordinate input points).
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvps: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2 (Y coordinate input values).
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvps: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3 (The period).
 */
  p = (float *) NclGetArgValue(
          2,
          5,
          &ndims_p,
          dsizes_p,
          NULL,
          NULL,
          &type_p,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_p != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_p != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvps: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }
/*
 * Retrieve argument #4 (The observation weights).
 */
  d = (float *) NclGetArgValue(
          3,
          5,
          &ndims_d,
          dsizes_d,
          NULL,
          NULL,
          &type_d,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_d != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_d != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvps: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5 (the X coordinate output values).
 */
  xo = (float *) NclGetArgValue(
          4,
          5,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftcurvps: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftcurvps: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Return the interpolated smooth curve.
 */
  yo = (float *) calloc(dsizes_xo[0], sizeof(float)); 
  isw = 1;
  if (dsizes_d[0] > 1) isw = 0;
  fterr = c_ftcurvps(dsizes_xi[0], xi, yi, *p, isw, d, dsizes_xo[0], xo, yo);
  if (fterr != 0) {
    sprintf(ftmsg, "ftcurvps: Error number %d.", fterr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, ftmsg);
    return(NhlFATAL);
  }
  return(NclReturnValue( 
                         (void *) yo, 
                         1,
                         dsizes_xo,
                         NULL,
                         NCL_float,
                         0 
                       )
        );
}

NhlErrorTypes ftkurv_W(void)
{

/*
 * Input array variables
 */
  float *xi;
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int ndims_ti, dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xi, type_yi, type_ti, type_xo, type_yo;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurv: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurv: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          5,
          &ndims_ti,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ti != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_ti != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurv: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurv: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          &ndims_yo,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurv: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurv: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurv(dsizes_xi[0], xi, yi, dsizes_xo[0], ti, xo, yo);
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
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int ndims_ti, dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xi, type_yi, type_ti, type_xo, type_yo;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(
          0,
          5,
          &ndims_xi,
          dsizes_xi,
          NULL,
          NULL,
          &type_xi,
          2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvp: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvp: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          5,
          &ndims_ti,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ti != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_ti != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvp: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvp: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          &ndims_yo,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvp: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvp: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 *  Invoke the C function.
 */
  fterr = c_ftkurvp(dsizes_xi[0], xi, yi, dsizes_xo[0], ti, xo, yo);
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
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int ndims_ti, dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *xd;
  int ndims_xd, dsizes_xd[NCL_MAX_DIMENSIONS];
  float *yd;
  int ndims_yd, dsizes_yd[NCL_MAX_DIMENSIONS];
  float *xdd;
  int ndims_xdd, dsizes_xdd[NCL_MAX_DIMENSIONS];
  float *ydd;
  int ndims_ydd, dsizes_ydd[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xi, type_yi, type_ti, type_xo, type_yo,
                    type_xd, type_yd, type_xdd, type_ydd;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(0,9,&ndims_xi,dsizes_xi,NULL,NULL,&type_xi,2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          9,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          9,
          &ndims_ti,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ti != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_ti != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          9,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          9,
          &ndims_yo,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #6 - xd
 */
  xd = (float *) NclGetArgValue(
          5,
          9,
          &ndims_xd,
          dsizes_xd,
          NULL,
          NULL,
          &type_xd,
          2);

/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_xd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
      if (type_xd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #6 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #7 - yd
 */
  yd = (float *) NclGetArgValue(
          6,
          9,
          &ndims_yd,
          dsizes_yd,
          NULL,
          NULL,
          &type_yd,
          2);

/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_yd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
      if (type_yd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #6 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #8 - xdd
 */
  xdd = (float *) NclGetArgValue(
          7,
          9,
          &ndims_xdd,
          dsizes_xdd,
          NULL,
          NULL,
          &type_xdd,
          2);

/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_xdd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
      if (type_xdd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #8 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #9 - ydd
 */
  ydd = (float *) NclGetArgValue(
          8,
          9,
          &ndims_ydd,
          dsizes_ydd,
          NULL,
          NULL,
          &type_ydd,
          2);

/*
 * Check number of dimensions for argument #9.
 */
  if(ndims_ydd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvd: Argument #9 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #9.
 */
      if (type_ydd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvd: Argument #9 has an incorrect type.");
        return(NhlFATAL);
      }

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
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *ti;
  int ndims_ti, dsizes_ti[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *xd;
  int ndims_xd, dsizes_xd[NCL_MAX_DIMENSIONS];
  float *yd;
  int ndims_yd, dsizes_yd[NCL_MAX_DIMENSIONS];
  float *xdd;
  int ndims_xdd, dsizes_xdd[NCL_MAX_DIMENSIONS];
  float *ydd;
  int ndims_ydd, dsizes_ydd[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xi, type_yi, type_ti, type_xo, type_yo,
                    type_xd, type_yd, type_xdd, type_ydd;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(0,9,&ndims_xi,dsizes_xi,NULL,NULL,&type_xi,2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          9,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  ti = (float *) NclGetArgValue(
          2,
          9,
          &ndims_ti,
          dsizes_ti,
          NULL,
          NULL,
          &type_ti,
          2);

/*
 * Check number of dimensions for argument #3.
 */
  if(ndims_ti != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #3 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
      if (type_ti != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #3 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          9,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          9,
          &ndims_yo,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #6 - xd
 */
  xd = (float *) NclGetArgValue(
          5,
          9,
          &ndims_xd,
          dsizes_xd,
          NULL,
          NULL,
          &type_xd,
          2);

/*
 * Check number of dimensions for argument #6.
 */
  if(ndims_xd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #6 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #6.
 */
      if (type_xd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #6 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #7 - yd
 */
  yd = (float *) NclGetArgValue(
          6,
          9,
          &ndims_yd,
          dsizes_yd,
          NULL,
          NULL,
          &type_yd,
          2);

/*
 * Check number of dimensions for argument #7.
 */
  if(ndims_yd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #7 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #7.
 */
      if (type_yd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #6 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #8 - xdd
 */
  xdd = (float *) NclGetArgValue(
          7,
          9,
          &ndims_xdd,
          dsizes_xdd,
          NULL,
          NULL,
          &type_xdd,
          2);

/*
 * Check number of dimensions for argument #8.
 */
  if(ndims_xdd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #8 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #8.
 */
      if (type_xdd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #8 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #9 - ydd
 */
  ydd = (float *) NclGetArgValue(
          8,
          9,
          &ndims_ydd,
          dsizes_ydd,
          NULL,
          NULL,
          &type_ydd,
          2);

/*
 * Check number of dimensions for argument #9.
 */
  if(ndims_ydd != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftkurvpd: Argument #9 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #9.
 */
      if (type_ydd != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftkurvpd: Argument #9 has an incorrect type.");
        return(NhlFATAL);
      }

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
  int ndims_xi, dsizes_xi[NCL_MAX_DIMENSIONS];
  float *yi;
  int ndims_yi, dsizes_yi[NCL_MAX_DIMENSIONS];
  float *zi;
  int ndims_zi, dsizes_zi[NCL_MAX_DIMENSIONS];
  float *xo;
  int ndims_xo, dsizes_xo[NCL_MAX_DIMENSIONS];
  float *yo;
  int ndims_yo, dsizes_yo[NCL_MAX_DIMENSIONS];
  float *zo;
  int ndims_zo, dsizes_zo[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_xi, type_yi, type_zi, type_xo, type_yo, type_zo;

  int i,ji,jo,k,nxi,nyi,nxo,nyo,nt;
  float *ztmp;

/*
 * Retrieve argument #1
 */
  xi = (float *) NclGetArgValue(0,5,&ndims_xi,dsizes_xi,NULL,NULL,&type_xi,2);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_xi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #1.
 */
      if (type_xi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftsurf: Argument #1 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #2
 */
  yi = (float *) NclGetArgValue(
          1,
          5,
          &ndims_yi,
          dsizes_yi,
          NULL,
          NULL,
          &type_yi,
          2);

/*
 * Check number of dimensions for argument #2.
 */
  if(ndims_yi != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #2 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #2.
 */
      if (type_yi != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftsurf: Argument #2 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #3
 */
  zi = (float *) NclGetArgValue(
          2,
          5,
          &ndims_zi,
          dsizes_zi,
          NULL,
          NULL,
          &type_zi,
          2);

/*
 * Check number of dimensions for argument #3.  This argument must
 * have at least two dimensions, but can have more.
 *
 */
  if(ndims_zi < 2) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #3 has less than two dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #3.
 */
  if (type_zi != NCL_float) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, 
            "ftsurf: Argument #3 has an incorrect type.");
    return(NhlFATAL);
  }


/*
 * Retrieve argument #4
 */
  xo = (float *) NclGetArgValue(
          3,
          5,
          &ndims_xo,
          dsizes_xo,
          NULL,
          NULL,
          &type_xo,
          2);

/*
 * Check number of dimensions for argument #4.
 */
  if(ndims_xo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #4.
 */
      if (type_xo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftsurf: Argument #4 has an incorrect type.");
        return(NhlFATAL);
      }

/*
 * Retrieve argument #5
 */
  yo = (float *) NclGetArgValue(
          4,
          5,
          &ndims_yo,
          dsizes_yo,
          NULL,
          NULL,
          &type_yo,
          2);

/*
 * Check number of dimensions for argument #5.
 */
  if(ndims_yo != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "ftsurf: Argument #5 has the wrong number of dimensions.");
    return(NhlFATAL);
  }

/*
 * Check the argument type for argument #5.
 */
      if (type_yo != NCL_float) {
        NhlPError(NhlFATAL, NhlEUNKNOWN, 
                "ftsurf: Argument #5 has an incorrect type.");
        return(NhlFATAL);
      }

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

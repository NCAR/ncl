#include <stdio.h>
#include <string.h>
#include "wrapper.h"

NhlErrorTypes nggcog_W(void)
{

/*
 *  Input variables
 */

  float     *clat, *clon, *crad;

/*
 *  Output variables
 */
  float     *alat, *alon;
  ng_size_t  dsizes_alat[1];
  ng_size_t  dsizes_alon[1];

/*
 *  Local variables.
 */
  ng_size_t num_points;
  int       inum_points;

/*
 *  Retrieve argument #1 (input)
 */
  clat = (float *) NclGetArgValue(
      0,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #2 (input)
 */
  clon = (float *) NclGetArgValue(
      1,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #3 (input)
 */
  crad = (float *) NclGetArgValue(
      2,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #4 (output)
 */
  alat = (float *) NclGetArgValue(
       3,
       5,
       NULL,
       dsizes_alat,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Retrieve argument #5 (output)
 */
  alon = (float *) NclGetArgValue(
       4,
       5,
       NULL,
       dsizes_alon,
       NULL,
       NULL,
       NULL,
       1);

/*
 *  Check that the two output arrays are of the same size.
 */
  if (dsizes_alat[0] != dsizes_alon[0]) {
         NhlPError(NhlFATAL,NhlEUNKNOWN,
            "nggcog: output arrays must be of the same size");
         return(NhlFATAL);
  }
  else {
        num_points = dsizes_alat[0];
  }

/*
 * Test dimension sizes.
 */
  if(num_points > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nggcog: the length of lat/lon is > INT_MAX");
    return(NhlFATAL);
  }
  inum_points = (int) num_points;

/*
 *  Make the call to c_nggcog.
 */
    c_nggcog(*clat, *clon, *crad, alat, alon, inum_points);
    return(NhlNOERROR);
}


NhlErrorTypes ngsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"ca", "cl", "cm", "co", "ct", "er", 
                      "fu", "ig", "jo", "lb", "lc", "lt", 
                      "lx", "ly", "mc", "pa", "pc", "pe", 
                      "sc", "ss", "st", "ux", "uy", "wo",
                      "ph", "pw", "sh", "sw",
                      "CA", "CL", "CM", "CO", "CT", "ER", 
                      "FU", "IG", "JO", "LB", "LC", "LT", 
                      "LX", "LY", "MC", "PA", "PC", "PE", 
                      "SC", "SS", "ST", "UX", "UY", "WO",
                      "PH", "PW", "SH", "SW"
                     };

  char *params_f[] = {"fi", "ha", "mi", "no", "os", "ox", "oy", 
                      "FI", "HA", "MI", "NO", "OS", "OX", "OY"
                     };

  char *params_c[] = {"me", "pi", "se",
                      "ME", "PI", "SE"
                     };

/*
 * Input array variables
 */
  NrmQuark *pname;
  void *pvalue;
  NclBasicDataTypes type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          2,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "ngsetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 * Retrieve argument #2
 */
OK_NAME: pvalue = (void *) NclGetArgValue(
           1,
           2,
           NULL,
           NULL,
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
        c_ngseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ngsetp: The specified value for the "
              "parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_ngsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_ngsetr(arg1, (float) *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ngsetp: The specified value for the "
             "parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((NrmQuark *) pvalue));
        c_ngsetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ngsetp: The specified value for the "
              "parameter has an invalid type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "ngsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
}


NhlErrorTypes nggetp_W(void)
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
  char *params_i[] = {"ca", "cl", "cm", "co", "ct", "er", 
                      "fu", "ig", "jo", "lb", "lc", "lt", 
                      "lx", "ly", "mc", "pa", "pc", "pe", 
                      "sc", "ss", "st", "ux", "uy", "wo",
                      "ph", "pw", "sh", "sw",
                      "CA", "CL", "CM", "CO", "CT", "ER", 
                      "FU", "IG", "JO", "LB", "LC", "LT", 
                      "LX", "LY", "MC", "PA", "PC", "PE", 
                      "SC", "SS", "ST", "UX", "UY", "WO",
                      "PH", "PW", "SH", "SW"
                     };

  char *params_f[] = {"fi", "ha", "mi", "no", "os", "ox", "oy", 
                      "FI", "HA", "MI", "NO", "OS", "OX", "OY"
                     };

  char *params_c[] = {"me", "pi", "se",
                      "ME", "PI", "SE"
                     };

/*
 * Input array variable
 */
  NrmQuark *pname;
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
          NULL,
          NULL,
          NULL,
          NULL,
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nggetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_nggeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_nggetr(arg1, fval);
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
             "nggetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_nggetc(arg1, cval, 99);
      qvalue = (NrmQuark *) calloc(1,sizeof(NrmQuark));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nggetp: impossible to get this message");
  return(NhlFATAL);
}


NhlErrorTypes ngritd_W(void)
{
/*
 *  Input variables
 */
  float *angl, *ucrd, *vcrd, *wcrd;
  int *iaxs;

/*
 *  Retrieve argument #1 (input)
 */
  iaxs = (int *) NclGetArgValue(
      0,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #2 (input)
 */
  angl = (float *) NclGetArgValue(
      1,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #3 (input)
 */
  ucrd = (float *) NclGetArgValue(
      2,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      NULL,
      DONT_CARE);

/*
 *  Retrieve argument #4 (output)
 */
  vcrd = (float *) NclGetArgValue(
       3,
       5,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);

/*
 *  Retrieve argument #5 (output)
 */
  wcrd = (float *) NclGetArgValue(
       4,
       5,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       DONT_CARE);
/*
 *  Make the call to c_ngritd.
 */
  c_ngritd(*iaxs, *angl, ucrd, vcrd, wcrd);
  return(NhlNOERROR);
}



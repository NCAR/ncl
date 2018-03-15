#include <stdio.h>
#include <string.h>
#include "wrapper.h"

NhlErrorTypes nggcog_W(void)
{
/*
 *  Input variables
 */

  void     *lat, *lon, *rad;
  void     *tmp_lat, *tmp_lon, *tmp_rad;
  NclBasicDataTypes type_lat, type_lon, type_rad;

/*
 *  Output variables
 */
  void      *olat, *olon;
  void      *tmp_olat, *tmp_olon;
  ng_size_t  dsizes_olat[1];
  ng_size_t  dsizes_olon[1];
  NclBasicDataTypes type_olat, type_olon;

/*
 *  Local variables.
 */
  ng_size_t i, num_points;
  int       inum_points;
  NclBasicDataTypes type_calc = NCL_float;

/*
 *  Retrieve argument #1 (input)
 */
  lat = (void *) NclGetArgValue(
      0,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      &type_lat,
      DONT_CARE);

/*
 *  Retrieve argument #2 (input)
 */
  lon = (void *) NclGetArgValue(
      1,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      &type_lon,
      DONT_CARE);

/*
 *  Retrieve argument #3 (input)
 */
  rad = (void *) NclGetArgValue(
      2,
      5,
      NULL,
      NULL,
      NULL,
      NULL,
      &type_rad,
      DONT_CARE);

/*
 *  Retrieve argument #4 (output)
 */
  olat = (void *) NclGetArgValue(
       3,
       5,
       NULL,
       dsizes_olat,
       NULL,
       NULL,
       &type_olat,
       1);

/*
 *  Retrieve argument #5 (output)
 */
  olon = (void *) NclGetArgValue(
       4,
       5,
       NULL,
       dsizes_olon,
       NULL,
       NULL,
       &type_olon,
       1);

/*
 * Check if any arguments are double.
 */
  if(type_lat  == NCL_double || type_lon  == NCL_double || type_rad == NCL_double ||
     type_olat == NCL_double || type_olon == NCL_double) type_calc = NCL_double;

/*
 *  Check that the two output arrays are of the same size.
 */
  if(dsizes_olat[0] != dsizes_olon[0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nggcog: output arrays must be of the same size");
    return(NhlFATAL);
  }
  else {
        num_points = dsizes_olat[0];
  }

  if((type_olat != NCL_float && type_olat != NCL_double) ||
     (type_olon != NCL_float && type_olon != NCL_double)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nggcog: The output arrays must be float or double");
    return(NhlFATAL);
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
 * Coerce input scalars as appropriate.
 */
  if(type_calc == NCL_double) {
    if(type_lat == NCL_float) {
      tmp_lat = (double*)calloc(1,sizeof(double));
      *((double *)tmp_lat) = (double)((float*)lat)[0];
    } else {
      tmp_lat = coerce_input_double(lat,type_lat,1,0,NULL,NULL);
    }

    if(type_lon == NCL_float) {
      tmp_lon = (double*)calloc(1,sizeof(double));
      *((double *)tmp_lon) = (double)((float*)lon)[0];
    } else {
      tmp_lon = coerce_input_double(lon,type_lon,1,0,NULL,NULL);
    }

    if(type_rad == NCL_float) {
      tmp_rad = (double*)calloc(1,sizeof(double));
      *((double *)tmp_rad) = (double)((float*)rad)[0];
    } else {
      tmp_rad = coerce_input_double(rad,type_rad,1,0,NULL,NULL);
    }
  } else {
      tmp_lat = coerce_input_float(lat,type_lat,1,0,NULL,NULL);
      tmp_lon = coerce_input_float(lon,type_lon,1,0,NULL,NULL);
      tmp_rad = coerce_input_float(rad,type_rad,1,0,NULL,NULL);
  }

/*
 * Create temp arrays to hold temporary output float/double arrays, if necessary.
 */
  if(type_calc == NCL_double) {
    if(type_olat == NCL_float)  tmp_olat = (double*)calloc(num_points,sizeof(double));
    else                        tmp_olat = (double*)olat;
    if(type_olon == NCL_float)  tmp_olon = (double*)calloc(num_points,sizeof(double));
    else                        tmp_olon = (double*)olon;
  } else {
                                tmp_olat = (float*)olat;
                                tmp_olon = (float*)olon;
  }

/*
 * Check for unallocated arrays
 */
  if(tmp_olat == NULL || tmp_olon == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"nggcog: unable to allocate memory for temporary output arrays");
    return(NhlFATAL);
  }

/*
 * Make the call to c_nggcog.
 */
  if(type_calc == NCL_double) {
    c_dnggcog(*((double *)tmp_lat),*((double *)tmp_lon),*((double *)tmp_rad),(double *)tmp_olat,(double *)tmp_olon, inum_points);
  } else {
    c_nggcog(*((float *)tmp_lat),*((float *)tmp_lon),*((float *)tmp_rad),(float *)tmp_olat,(float *)tmp_olon, inum_points);
  }

/*
 * Coerce output to double if needed
 */
  if(type_calc == NCL_double) {
    if(type_olat == NCL_float) {
      for( i = 0; i < num_points; i++ ) ((float*)olat)[i]  = (float)((double *)tmp_olat)[i];
      NclFree(tmp_olat);
    }
    if(type_olon == NCL_float) {
      for( i = 0; i < num_points; i++ ) ((float*)olon)[i]  = (float)((double *)tmp_olon)[i];
      NclFree(tmp_olon);
    }
    if(type_lat != NCL_double) NclFree(tmp_lat);
    if(type_lon != NCL_double) NclFree(tmp_lon);
    if(type_rad != NCL_double) NclFree(tmp_rad);
  } else {
    if(type_lat != NCL_float) NclFree(tmp_lat);
    if(type_lon != NCL_float) NclFree(tmp_lon);
    if(type_rad != NCL_float) NclFree(tmp_rad);
  }

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

#include <stdio.h>
#include <string.h>
#include "wrapper.h"

extern int   *c_csstri(int, float [], float [], int *, int *);
extern float *c_cssgrid(int, float [], float [], float [],
                 int, int, float [], float [], int *);
extern void   c_csvoro(int, float [], float [], int, int,
                float [], float [], float [], int *,
                int *, int [], int *);
extern void   c_css2c(int, float *, float *, float *, float *, float *);
extern void   c_csc2s(int, float *, float *, float *, float *, float *);
extern void   c_cssetr(char *, float);
extern void   c_csgetr(char *, float *);
extern void   c_csseti(char *, int);
extern void   c_csgeti(char *, int *);

extern int   *c_csstrid(int, double [], double [], int *, int *);
extern double *c_cssgridd(int, double [], double [], double [],
                 int, int, double [], double [], int *);
extern void   c_csvorod(int, double [], double [], int, int,
                double [], double [], double [], int *,
                int *, int [], int *);
extern void   c_csc2sd(int, double *, double *, double *, double *, double *);
extern void   c_css2cd(int, double *, double *, double *, double *, double *);
extern void   c_cssetd(char *, double);
extern void   c_csgetd(char *, double *);

int cserr;
char csmsg[61];

NhlErrorTypes css2c_W(void)
{

  int ndims[2];
  ng_size_t dsizes[2][NCL_MAX_DIMENSIONS];
  int has_missing[2],dflag;
  ng_size_t return_dsizes[NCL_MAX_DIMENSIONS];
  NclScalar missing[2],missingd[2],return_missing,*missing_ptr;
  void *datav[2];
  double platd, plond;
  double *return_value_d = NULL;
  float  platf, plonf, mvalf;
  float  *return_value_f = NULL;

  ng_size_t i,j,nt;

  NclBasicDataTypes atypes[2];  

  for (i = 0; i < 2; i++) {
    datav[i] = NclGetArgValue(i,2,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check on the argument types.  If either of the lat/lon input
 *  arrays is double, then the output array will be returned as
 *  double.  In all other cases the output array will be returned
 *  as a float.
 *
 */
  dflag = 0;
  if ( (atypes[0] == NCL_double) || (atypes[1] == NCL_double)) {
    dflag = 1;
  }

/*
 *  Check that all dimensions are the same.
 */
  for (i = 1; i < 2; i++) {
    if (ndims[i] != ndims[0]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"css2c: all arguments "
                "must have the same number of dimensions\n");
      return(NhlFATAL);
    }
  }

/*
 *  Check that all dimension sizes are the same.  
 */
  for (j = 0; j < ndims[0]; j++) {
    for (i = 1; i < 2; i++) {
      if (dsizes[i][j] != dsizes[0][j]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"css2c: all arguments "
                  "must have the same dimension sizes\n");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Calculate the total number of coordinates to convert.
 */
  nt = 1;
  for (i = 0; i < ndims[0]; i++) {
    nt *= dsizes[0][i];
  }

/*
 *  Coerce the missing values to double.
 */
  for (i = 0; i < 2; i++) {
    coerce_missing(atypes[i],has_missing[i],missing+i,missingd+i,NULL);
  }

/*
 *  Determine what to return as the missing value if one is
 *  required.  This will be the value set for the parameter
 *  MVL if it is set, otherwise it will be the missing value
 *  of the first argument, unless the first argument has no
 *  missing values, but the second argument does, in which
 *  case the returned missing value is that of the second
 *  argument.  The default value for the MVL parameter is -8.,
 *  this is used simply as a flag to see if a value has been
 *  set for that parameter.
 */
  c_csgetr("MVL",&mvalf);
  if (mvalf == -8.) {
    if (has_missing[0]) { 
      if (dflag) {
        return_missing.doubleval = missingd[0].doubleval;
      }
      else {
        return_missing.floatval = (float) missingd[0].doubleval;
      }
      missing_ptr = &return_missing;
    }
    else if (has_missing[1]) {
      if (dflag) {
        return_missing.doubleval = missingd[1].doubleval;
      }
      else {
        return_missing.floatval = (float) missingd[1].doubleval;
      }
      missing_ptr = &return_missing;
    }
    else {
      missing_ptr = (NclScalar *) NULL;
    }
  }
  else {
    if (dflag) {
      return_missing.doubleval = (double) mvalf;
    }
    else {
      return_missing.floatval =  mvalf;
    }
    missing_ptr = &return_missing;
  }

/*
 *  Calculate the Cartesian coordinates one at a time,
 *  checking for missing values along the way.
 */
  if (dflag) {
    return_value_d = (double *) calloc(3*nt,sizeof(double));
  }
  else {
    return_value_f = (float *) calloc(3*nt,sizeof(float));
  }

  for (i = 0; i < nt; i++) {
    coerce_subset_input_double(datav[0],&platd,i,atypes[0],1,
                               has_missing[0],missing,missingd);
    coerce_subset_input_double(datav[1],&plond,i,atypes[1],1,
                               has_missing[1],missing+1,missingd+1);

    if ( (has_missing[0] && (platd == missingd[0].doubleval)) ||
         (has_missing[1] && (plond == missingd[1].doubleval))) {
      if (dflag) {
        return_value_d[     i] = return_missing.doubleval;
        return_value_d[  nt+i] = return_missing.doubleval;
        return_value_d[2*nt+i] = return_missing.doubleval;
      }
      else {
        return_value_f[     i] = return_missing.floatval;
        return_value_f[  nt+i] = return_missing.floatval;
        return_value_f[2*nt+i] = return_missing.floatval;
      }
    }
    else {
      if (dflag) {
        c_css2cd(1, &platd, &plond, 
                 return_value_d+i, return_value_d+nt+i, return_value_d+2*nt+i);
      }
      else {
        platf = (float) platd;
        plonf = (float) plond;
        c_css2c(1, &platf, &plonf, 
                 return_value_f+i, return_value_f+nt+i, return_value_f+2*nt+i);
      }
    }
  }

/*
 *  Return.
 */
  return_dsizes[0] = 3;
  for (i = 0; i < ndims[0]; i++) {
    return_dsizes[i+1] = dsizes[0][i];
  }
  
  if (dflag) {
    NclReturnValue( (void *) return_value_d, ndims[0]+1, 
                    return_dsizes, missing_ptr, NCL_double,0);
  }
  else {
    NclReturnValue( (void *) return_value_f, ndims[0]+1, 
                    return_dsizes, missing_ptr, NCL_float,0);
  }
  return(NhlNOERROR);
}

NhlErrorTypes csc2s_W(void)
{

  int ndims[3];
  ng_size_t dsizes[3][NCL_MAX_DIMENSIONS];
  int has_missing[3],dflag;
  ng_size_t return_dsizes[NCL_MAX_DIMENSIONS];
  NclScalar missing[3],missingd[3],return_missing,*missing_ptr;
  void *datav[3];
  double xid,yid,zid;
  double *return_value_d = NULL;
  float   xi, yi, zi, mvalf;
  float   *return_value_f = NULL;

  ng_size_t i,j,nt;

  NclBasicDataTypes atypes[3];  

  for (i = 0; i < 3; i++) {
    datav[i] = NclGetArgValue(i,3,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check on the argument types.  If any of the input
 *  arrays is double, then the output array will be returned as
 *  double.  In all other cases the output array will be returned
 *  as a float.  
 *
 */
  dflag = 0;
  if ( (atypes[0] == NCL_double) || (atypes[1] == NCL_double) ||
       (atypes[2] == NCL_double)) {
    dflag = 1;
  }

/*
 *  Check that all dimensions are the same.
 */
  for (i = 1; i < 3; i++) {
    if (ndims[i] != ndims[0]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csc2s: all arguments "
                "must have the same number of dimensions\n");
      return(NhlFATAL);
    }
  }

/*
 *  Check that all dimension sizes are the same.  
 */
  for (j = 0; j < ndims[0]; j++) {
    for (i = 1; i < 3; i++) {
      if (dsizes[i][j] != dsizes[0][j]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"csc2s: all arguments "
                  "must have the same dimension sizes\n");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Calculate the total number of coordinates to convert.
 */
  nt = 1;
  for (i = 0; i < ndims[0]; i++) {
    nt *= dsizes[0][i];
  }

/*
 *  Coerce the missing values to double.
 */
  for (i = 0; i < 3; i++) {
    coerce_missing(atypes[i],has_missing[i],missing+i,missingd+i,NULL);
  }

/*
 *  Determine what to return as the missing value if one is
 *  required.  This will be the value set for the parameter
 *  MVL if it is set, otherwise it will be the missing value
 *  of the first argument, unless the first argument has no
 *  missing values, but the second argument does, in which
 *  case the returned missing value is that of the second
 *  argument, and so on for the third argument.  The default 
 *  value for the MVL parameter is -8., this is used simply 
 *  as a flag to see if a value has been set for that parameter.
 */
  c_csgetr("MVL",&mvalf);
  if (mvalf == -8.) {
    if (has_missing[0]) {
      if (dflag) {
        return_missing.doubleval = missingd[0].doubleval;
      }
      else {
        return_missing.floatval = (float) missingd[0].doubleval;
      }
      missing_ptr = &return_missing;
    }
    else if (has_missing[1]) {
      if (dflag) {
        return_missing.doubleval = missingd[1].doubleval;
      }
      else {
        return_missing.floatval = (float) missingd[1].doubleval;
      }
      missing_ptr = &return_missing;
    }
    else if (has_missing[2]) {
      if (dflag) {
        return_missing.doubleval = missingd[2].doubleval;
      }
      else {
        return_missing.floatval = (float) missingd[2].doubleval;
      }
      missing_ptr = &return_missing;
    }
    else {
      missing_ptr = (NclScalar *) NULL;
    }
  }
  else {
    if (dflag) {
      return_missing.doubleval = (double) mvalf;
    }
    else {
      return_missing.floatval =  mvalf;
    }
    missing_ptr = &return_missing;
  }

/*
 *  Calculate the lat/lon coordinates one at a time,
 *  checking for missing values along the way.
 */
  if (dflag) {
    return_value_d = (double *) calloc(2*nt,sizeof(double));
  }
  else {
    return_value_f = (float *) calloc(2*nt,sizeof(float));
  }

  for (i = 0; i < nt; i++) {
    coerce_subset_input_double(datav[0],&xid,i,atypes[0],1,
                               has_missing[0],missing,missingd);
    coerce_subset_input_double(datav[1],&yid,i,atypes[1],1,
                               has_missing[1],missing+1,missingd+1);
    coerce_subset_input_double(datav[2],&zid,i,atypes[2],1,
                               has_missing[2],missing+2,missingd+2);

    if ( (has_missing[0] && (xid == missingd[0].doubleval)) ||
         (has_missing[1] && (yid == missingd[1].doubleval)) ||
         (has_missing[2] && (zid == missingd[2].doubleval))) {
      if (dflag) {
        return_value_d[   i] = return_missing.doubleval;
        return_value_d[nt+i] = return_missing.doubleval;
      }
      else {
        return_value_f[   i] = return_missing.floatval;
        return_value_f[nt+i] = return_missing.floatval;
      }
    }
    else {
      if (dflag) {
        c_csc2sd(1, &xid, &yid, &zid,
                 return_value_d+i, return_value_d+nt+i);
      }
      else {
        xi = (float) xid;
        yi = (float) yid;
        zi = (float) zid;
        c_csc2s(1, &xi, &yi, &zi,
                 return_value_f+i, return_value_f+nt+i);
      }
    }
  }

/*
 *  Return.
 */
  return_dsizes[0] = 2;
  for (i = 0; i < ndims[0]; i++) {
    return_dsizes[i+1] = dsizes[0][i];
  }

  if (dflag) {
    NclReturnValue( (void *) return_value_d, ndims[0]+1,
                    return_dsizes, missing_ptr, NCL_double,0);
  }
  else {
    NclReturnValue( (void *) return_value_f, ndims[0]+1,
                    return_dsizes, missing_ptr, NCL_float,0);
  }
  return(NhlNOERROR);
}

NhlErrorTypes cssetp_W(void)
{

  char  *arg1;
  int   numpi, numpf, numpd, i;

/*
 *  List the parameter names (integer, float, double).  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"nls", "NLS", "nsg", "NSG", "isg", "ISG", "igr", "IGR",
                      "mvl", "MVL"};
  char *params_f[] = {"sig", "SIG", "tol", "TOL", "mvl", "MVL", "ttf", "TTF"};
  char *params_d[] = {"dsg", "DSG", "dtl", "DTL", "dmv", "DMV", "ttd", "TTD",
                      "mvl", "MVL"};

/*
 * Input array variables
 */
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int ndims_pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname, type_pvalue;

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          2,
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "cssetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
  arg1 = NrmQuarkToString(*pname);
 
/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpd = sizeof(params_d)/sizeof(void *);
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
  for (i = 0; i < numpd; i++) {
    if (!strncmp(arg1, params_d[i], strlen(params_d[i]))) {
      goto OK_NAME;
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "cssetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 * Retrieve argument #2
 */
OK_NAME: pvalue = (void *) NclGetArgValue(
           1,
           2,
           &ndims_pvalue,
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
        c_csseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "cssetp - the specified value for"
              " the parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float) {
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        c_cssetr(arg1, *((float *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "cssetp - the specified value for"
              " the parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_double) {
    for (i = 0; i < numpd; i++) {
      if (!strncmp(arg1, params_d[i], strlen(params_d[i]))) {
        c_cssetd(arg1, *((double *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "cssetp - the specified value for"
              " the parameter has an invalid type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "cssetp - the specified value for"
              " the parameter has an invalid type");
    return(NhlFATAL);
  }
}

NhlErrorTypes csgetp_W(void)
{
/*
 *  Get values for csgrid parameters.
 */

  char  *arg1;
  int   numpi, numpf, numpd, i;

/*
 *  List the parameter names (integer, float, double).  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"nls", "NLS", "nsg", "NSG", "isg", "ISG", "igr", "IGR"};
  char *params_f[] = {"sig", "SIG", "tol", "TOL","mvl","TTF"};
  char *params_d[] = {"dsg", "DSG", "dtl", "DTL","dmv","TTD"};

/*
 * Input array variable
 */
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname;
  float  *fval;
  double *dval;
  int    *ival;
  ng_size_t    ret_size = 1; 

/*
 * Retrieve argument #1
 */
  pname = (NrmQuark *) NclGetArgValue(
          0,
          1,
          &ndims_pname,
          dsizes_pname,
          NULL,
          NULL,
          &type_pname,
          DONT_CARE);

/*
 * Check number of dimensions for argument #1.
 */
  if(ndims_pname != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "csgetp: Argument #1 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
  numpd = sizeof(params_d)/sizeof(void *);
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
  for (i = 0; i < numpd; i++) {
    if (!strncmp(arg1, params_d[i], strlen(params_d[i]))) {
      goto OK_NAME;
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "csgetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter.
 */
OK_NAME:  
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_csgeti(arg1,ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_csgetr(arg1,fval);
      return(NclReturnValue((void *)fval, 1, &ret_size, NULL, NCL_float, 0));
    }
  }
  for (i = 0; i < numpd; i++) {
    if (!strncmp(arg1, params_d[i], strlen(params_d[i]))) {
      dval = (double *) calloc(1,sizeof(double));
      c_csgetd(arg1,dval);
      return(NclReturnValue((void *)dval, 1, &ret_size, NULL, NCL_double, 0));
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "csgetp: impossible to get this message");
  return(NhlFATAL);
}


NhlErrorTypes csstri_W(void)
{

  int ndims[2];
  ng_size_t dsizes[2][NCL_MAX_DIMENSIONS];
  int has_missing[2];
  NclScalar missing[2],missingd[2];
  void *datav[2];
  double *platd, *plond, platdt, plondt;

  ng_size_t i,j,num_points;
  int nt, inum_points;
  int *trlist;
  ng_size_t dsizes_trlist[2];
  ng_size_t *indices;

  NclBasicDataTypes atypes[2];  

  for (i = 0; i < 2; i++) {
    datav[i] = NclGetArgValue(i,2,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that all dimensions are linear arrays.
 */
  for (i = 1; i < 2; i++) {
    if (ndims[i] != 1) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
                "must be linear arrays.\n");
      return(NhlFATAL);
    }
  }

/*
 *  Check that all dimension sizes are the same.  
 */
  for (j = 0; j < ndims[0]; j++) {
    for (i = 1; i < 2; i++) {
      if (dsizes[i][j] != dsizes[0][j]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
                  "must have the same dimension sizes\n");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Coerce the missing values to double.
 */
  for (i = 0; i < 2; i++) {
    coerce_missing(atypes[i],has_missing[i],missing+i,missingd+i,NULL);
  }

/*
 *  Create temporary arrays to hold lat/lon values that are
 *  not missing values.
 */
  platd = (double *) calloc(dsizes[0][0],sizeof(double));
  plond = (double *) calloc(dsizes[1][0],sizeof(double));

/*
 *  Create an array to store indices of non-missing values
 *  so that the triangle indices can be returned as indices
 *  to the original vertices rather than the vertices stored
 *  in platd and plond.
 */
  indices = (ng_size_t *) calloc(dsizes[0][0],sizeof(ng_size_t));

/*
 *  Loop through the input lat/lon values, coerce to double,
 *  cull out missing values, and store in platd and plond.
 */
  num_points = 0;
  for (i = 0; i < dsizes[0][0]; i++) {
    coerce_subset_input_double(datav[0],&platdt,i,atypes[0],1,
                               has_missing[0],missing,missingd);
    coerce_subset_input_double(datav[1],&plondt,i,atypes[1],1,
                               has_missing[1],missing+1,missingd+1);
    if ( (!has_missing[0] || (platdt !=  missingd[0].doubleval)) &&
         (!has_missing[1] || (plondt !=  missingd[1].doubleval))) {
      platd[num_points] = platdt;
      plond[num_points] = plondt;
      indices[num_points] = i;
      num_points++;
    }
  }

/*
 *  Issue warning if missing values have been detected.
 */
  if (num_points != dsizes[0][0]) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,
      "csstri: missing values in input points - those points ignored.");
  }

/*
 *  Issue an error if too few values left.
 */
  if (num_points < 3) {
    free (platd);
    free (plond);
    free (indices);
    NhlPError(NhlFATAL, NhlEUNKNOWN, 
      "csstri: number of valid input points less than three.\n");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if (num_points > INT_MAX) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, 
      "csstri: number of valid input points > INT_MAX.\n");
    return(NhlFATAL);
  }
  inum_points = (int) num_points;

/*
 *  Make the call to the C function.
 */
  trlist = c_csstrid(inum_points, platd, plond, &nt, &cserr);
  if (cserr != 0) {
    free (platd);
    free (plond);
    free (indices);
    sprintf(csmsg, "csstri: Error number %d.", cserr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
    return(NhlFATAL);
  }

  free (platd);
  free (plond);
  dsizes_trlist[0] = nt;
  dsizes_trlist[1] = 3;

/*
 *  Convert the vertex indices from the temporary array to the
 *  original array.
 */
  for (i = 0; i < 3*nt; i++) {
    trlist[i] = (int)indices[trlist[i]];
  }
  free (indices);
  return(NclReturnValue((void *) trlist, 2, dsizes_trlist, NULL, NCL_int, 0));

}

NhlErrorTypes csvoro_W(void)
{

  int ndims[10];
  ng_size_t dsizes[10][NCL_MAX_DIMENSIONS];
  int has_missing[10];
  int dflag;
  NclScalar missing[10],missingd[10];
  void *callp,*indexp;

  int i,callv,indexv;
  int np2;

  double platdt, plondt;
  NclBasicDataTypes atypes[10];

  static int num_points;
  static double *platd, *plond;
  static void *datav[10];
  static double *rlatd,*rlond, *rcd;


/*
 *  See if this is the first call to csvoro for the current
 *  dataset.  If so, retrieve and save all of the arrays
 *  except the vertex array.
 */
  callp = NclGetArgValue(3,10,ndims+3,&(dsizes[3][0]),missing+3,
                             has_missing+3,atypes+3,DONT_CARE);
  callv  = *((int *) callp);
  indexp = NclGetArgValue(2,10,ndims+2,&(dsizes[2][0]),missing+2,
                             has_missing+2,atypes+2,DONT_CARE);
  indexv = *((int *) indexp);

  if (callv == 1) {
    for (i = 0; i < 10; i++) {
      datav[i] = NclGetArgValue(i,10,ndims+i,&(dsizes[i][0]),missing+i,
                                has_missing+i,atypes+i,DONT_CARE);
    }

/*
 *  Check that the the lat/lon input are linear arrays.
 */
    for (i = 1; i < 2; i++) {
      if (ndims[i] != 1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: lat/lon input "
                  "must be linear arrays.\n");
        return(NhlFATAL);
      }
    }

/*
 *  Check that the dimensions of the first two arguments are the same.
 */
    if (dsizes[0][0] != dsizes[1][0]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: first two arguments must"
                " have the same dimension sizes\n");
      return(NhlFATAL);
    }
    num_points = (int) dsizes[0][0];

/*
 *  Check that arguments 4-6 (starting with argument 0) have 
 *  dimension sizes at least twice that of the size of the first 
 *  two arguments.
 */
    np2 = 2*num_points;
    if ((dsizes[4][0] < np2) || (dsizes[5][0] < np2) || (dsizes[6][0] < np2)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: arguments 4-6 must "
          "have dimension\n sizes at least twice that of the first argument\n");
      return(NhlFATAL);
    }

/*
 *  Coerce the missing values to double.
 */
    for (i = 0; i < 10; i++) {
      coerce_missing(atypes[i],has_missing[i],missing+i,missingd+i,NULL);
    }

/*
 *  Check on the argument types.  If either of the lat/lon input
 *  arrays is double, then the output vertices for the Voronoi
 *  polygons must be double.  In all other cases the output must
 *  be floats. 
 *
 */
    dflag = 0;
    if ( (atypes[0] == NCL_double) || (atypes[1] == NCL_double)) {
      dflag = 1;
    }
    if (dflag) {
      if ((atypes[4] != NCL_double) || (atypes[5] != NCL_double) ||
          (atypes[6] != NCL_double)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: arguments #4-6 must "
          "be double\n if either of arguments 0 or 1 is double.  Otherwise\n"
          "arguments #4-6 must be float\n");
      return(NhlFATAL);
      }
    }

/*
 *  Create temporary arrays to hold lat/lon values that are
 *  not missing values as well as space for the output arrays.
 */
    platd = (double *) calloc(dsizes[0][0],sizeof(double));
    plond = (double *) calloc(dsizes[1][0],sizeof(double));
    rlatd = (double *) calloc(dsizes[4][0],sizeof(double));
    rlond = (double *) calloc(dsizes[5][0],sizeof(double));
    rcd   = (double *) calloc(dsizes[6][0],sizeof(double));

/*
 *  Loop through the input lat/lon values, coerce to double,
 *  and check for missing values.
 */
    num_points = 0;
    for (i = 0; i < dsizes[0][0]; i++) {
      coerce_subset_input_double(datav[0],&platdt,i,atypes[0],1,
                                 has_missing[0],missing,missingd);
      coerce_subset_input_double(datav[1],&plondt,i,atypes[1],1,
                                 has_missing[1],missing+1,missingd+1);
      if ( (platdt !=  missingd[0].doubleval) &&
           (plondt !=  missingd[1].doubleval)) {
        platd[num_points] = platdt;
        plond[num_points] = plondt;
        num_points++;
      }
      else {
        free (platd);
        free (plond);
        free (rlatd);
        free (rlond);
        free (rcd);
        NhlPError(NhlFATAL, NhlEUNKNOWN,
          "csvoro: missing values not allowed in input.\n");
          return(NhlFATAL);
      }
    }
  }

/*
 *  Make the call to c_csvorod.
 */
  c_csvorod(num_points, platd, plond, indexv,
            *((int *) datav[3]), rlatd, rlond, rcd,
            (int *) datav[7], (int *) datav[8], (int *) datav[9], &cserr);
  if (cserr != 0) {
    free (platd);
    free (plond);
    free (rlatd);
    free (rlond);
    free (rcd);
    sprintf(csmsg, "csvoro: Error number %d.", cserr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
    return(NhlFATAL);
  }
  
  if (callv == 1) {
    if (dflag) {
      memcpy(datav[4], (void *) rlatd, dsizes[4][0]*sizeof(double));
      memcpy(datav[5], (void *) rlond, dsizes[5][0]*sizeof(double));
      memcpy(datav[6], (void *)   rcd, dsizes[6][0]*sizeof(double));
    }
    else {
      for (i = 0; i < dsizes[4][0]; i++) {
        *( (float *) datav[4]+i)  = rlatd[i];
        *( (float *) datav[5]+i)  = rlond[i];
        *( (float *) datav[6]+i)  =   rcd[i];
      }
    }
  }
  if (callv == -1) {
    free (platd);
    free (plond);
    free (rlatd);
    free (rlond);
    free (rcd);
  }
  return(NhlNOERROR);
}

NhlErrorTypes cssgrid_W(void)
{

  int ndims[5];
  ng_size_t dsizes[5][NCL_MAX_DIMENSIONS];
  int has_missing[5];
  NclScalar missing[5],missingd[5];
  void *datav[5];

  float     *zout = NULL;
  double    *zoutd = NULL;
  double    *platd,*plond,*fvald,*rlatd,*rlond,*platdt,*plondt,*fvaldt,*ztmp;
  ng_size_t nt, psize;
  int       zdim;
  ng_size_t zsize[NCL_MAX_DIMENSIONS];
  ng_size_t i,j,k,jo,ji,num_points;
  int       nxo,nyo,inum_points,num_missing,test_missing;

  NclBasicDataTypes atypes[5],ztype;

/*
 *  Get input values.
 */

  for (i = 0; i < 5; i++) {
    datav[i] = NclGetArgValue(i,5,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that the first two arguments are of the same size and
 *  the size of the rightmost dimension of argument 3 is that
 *  size as well.  Store the size of the rightmost dimension of 
 *  argument 3 (the size of each individual dataset interpolation)
 *  in psize
 */
  psize = dsizes[2][ndims[2]-1];
  if( (dsizes[0][0] != dsizes[1][0]) ||
      (dsizes[1][0] != psize)) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
           "cssgrid: the input lat/lon arrays must be the same size and must "
           "equal the size of the rightmost dimension of the data array.\n");
        return(NhlFATAL);
  }
  else {

/*
 * Test dimension sizes.
 */
    if(dsizes[0][0] > INT_MAX) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "cssgrid: number of possible valid points > INT_MAX\n");
      return(NhlFATAL);
    }
    num_points  = dsizes[0][0]; 
    inum_points = (int) num_points;
    num_missing = 0;
  }

/*
 *  Coerce the missing values to double.
 */
  for (i = 0; i < 5; i++) {
    coerce_missing(atypes[i],has_missing[i],missing+i,missingd+i,NULL);
  }

/*
 *  Coerce the input arrays to double precision, if they
 *  are not already that.
 */
  platd = coerce_input_double(datav[0],atypes[0],dsizes[0][0],
                            has_missing[0],missing,missingd);
  if (platd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for platd\n");
    return(NhlFATAL);
  }

  plond = coerce_input_double(datav[1],atypes[1],dsizes[1][0],
                            has_missing[1],missing,missingd);
  if (plond == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for plond\n");
    return(NhlFATAL);
  }

/*
 *  Calculate the total size of argument #3, the functional values, and
 *  coerce to double if necessary.  
 */
  nt = 1;
  for(j = 0; j < ndims[2]; j++) {
    nt *= dsizes[2][j];
  }

/*
 * Test dimension sizes.
 */
  if( (dsizes[3][0] > INT_MAX) || (dsizes[4][0] > INT_MAX)) { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: nxo and/or nyo > INT_MAX\n");
    return(NhlFATAL);
  }
  nxo = (int) dsizes[3][0];
  nyo = (int) dsizes[4][0];

/*
 *  Coerce the output grid to double and check for missing values.
 *  Missing values are not allowed in latitude or longitude arrays.
 */
  rlatd = coerce_input_double(datav[3],atypes[3],dsizes[3][0],
                            has_missing[3],missing,missingd);
  if (rlatd == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for rlatd\n");
    return(NhlFATAL);
  }

  rlond = coerce_input_double(datav[4],atypes[4],dsizes[4][0],
                            has_missing[4],missing+4,missingd+4);
  if (rlond == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for rlond\n");
    return(NhlFATAL);
  }

/*
 *  Check to see if there are missing values in the output lat/lon
 *  arrays.  These are not allowed.
 */
  test_missing = contains_missing(rlatd,dsizes[3][0],has_missing[3],
                                  missingd[3].doubleval);
  if (test_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: output latitude array is not "
          "allowed to contain a missing value.");
    return(NhlFATAL);
  }
  test_missing = contains_missing(rlond,dsizes[4][0],has_missing[4],
                                  missingd[4].doubleval);
  if (test_missing) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
          "cssgrid: output latitude array is not "
          "allowed to contain a missing value.");
    return(NhlFATAL);
  }

/*
 *  Set up the attributes of the output array and allocate space
 *  for temporary arrays to hold each individual dataset before
 *  interpolation.  These temporary arrays are an expeditious
 *  way of handling missing values in the input data.
 */
  zdim = ndims[2]+1;
  for(i = 0; i < ndims[2]-1; i++) {
    zsize[i] = dsizes[2][i];
  }
  zsize[ndims[2]-1] = nxo;
  zsize[ndims[2]]   = nyo;

/*
 *  Allocate space for the output array depending on
 *  whether it will be float or double.
 */
  if (atypes[2] == NCL_double) {
    ztype = NCL_double;
    zoutd = (double *) calloc(nxo*nyo*(nt/psize),sizeof(double));
    if (zoutd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for zoutd\n");
      return(NhlFATAL);
    }
  }
  else {
    ztype = NCL_float;
    zout =  (float *) calloc(nxo*nyo*(nt/psize),sizeof(float));
    if (zout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for zout\n");
      return(NhlFATAL);
    }
  }

/*
 *  Allocate space for temporary arrays that are just large
 *  enough to hold input data for a single interpolation.
 */
  platdt = (double *) calloc(psize,sizeof(double));
  if (platdt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for platdt\n");
    return(NhlFATAL);
  }
  plondt = (double *) calloc(psize,sizeof(double));
  if (plondt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for plondt\n");
    return(NhlFATAL);
  }
  fvaldt = (double *) calloc(psize,sizeof(double));
  if (fvaldt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for fvaldt\n");
    return(NhlFATAL);
  }
  fvald = (double *) calloc(psize,sizeof(double));
  if (fvald == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for fvald\n");
    return(NhlFATAL);
  }

/*
 *  Do the interpolations and fill the output array.
 */
  jo = 0;
  for (ji = 0; ji < nt; ji += psize) {
    num_points = 0;
    coerce_subset_input_double(datav[2],fvald,ji,atypes[2],psize,
                               has_missing[2],missing+2,missingd+2);
    for (i = 0; i < psize; i++) {
/*
 *  Check for missing values and discard any input point
 *  that has a missing value.
 */
      if ( (!has_missing[0] || (platd[i] !=  missingd[0].doubleval)) &&
           (!has_missing[1] || (plond[i] !=  missingd[1].doubleval)) &&
           (!has_missing[2] || (fvald[i] !=  missingd[2].doubleval))) {
        platdt[num_points] = platd[i];
        plondt[num_points] = plond[i];
        fvaldt[num_points] = fvald[i];
        num_points++;
      }
    }
    if (num_points < 3) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "cssgrid: missing values in the input data have reduced the number "
        "of valid points to less than 3.\n");
      return(NhlFATAL);
    }
/*
 * Test dimension sizes.
 */
    if(num_points > INT_MAX) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
                "cssgrid: number of valid points > INT_MAX\n");
      return(NhlFATAL);
    }
    inum_points = (int) num_points;

    ztmp = c_cssgridd(inum_points,platdt,plondt,fvaldt,nxo,nyo,
                      rlatd,rlond,&cserr);
    if (cserr != 0) {
      sprintf(csmsg, "cssgrid: Error number %d.", cserr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
      return(NhlFATAL);
    }

    if (ztype == NCL_double) {
      for (k = 0; k < nxo*nyo; k++) {
        zoutd[jo+k] = ztmp[k];
      }
    }
    else {
      for (k = 0; k < nxo*nyo; k++) {
        zout[jo+k] = (float) ztmp[k];
      }
    }
    free(ztmp);
    jo += nxo*nyo;
  }

/*
 *  Free allocated space.
 */
  if ( (void *) platd != datav[0]) {
    free (platd);
  }
  if ( (void *) plond != datav[1]) {
    free (plond);
  }
  if ( (void *) rlatd != datav[3]) {
    free (rlatd);
  }
  if ( (void *) rlond != datav[4]) {
    free (rlond);
  }
  free(platdt);
  free(plondt);
  free(fvaldt);
  free(fvald);

/*
 *  Return the array.
 */
  if (ztype == NCL_double) {
    return( NclReturnValue( (void*) zoutd, zdim, zsize, NULL, NCL_double, 0) );
  }
  else if (ztype == NCL_float) {
    return( NclReturnValue( (void*) zout, zdim, zsize, NULL, NCL_float, 0) );
  }
  return(NhlNOERROR);
}

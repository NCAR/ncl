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

#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include <ncarg/ncl/NclVar.h>
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
#include <ncarg/ncl/NclCallBacksI.h>


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

  int ndims[5], dsizes[5][NCL_MAX_DIMENSIONS], has_missing[5];
  NclScalar missing[5];
  void *datav[5];

  int i,j,nt;

  NclBasicDataTypes atypes[5];  

  for (i = 0; i < 5; i++) {
    datav[i] = NclGetArgValue(i,5,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that all dimensions are the same.
 */
  for (i = 1; i < 5; i++) {
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
    for (i = 1; i < 5; i++) {
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
 *  Check that all arguments are either all float or all double.
 */
  if ((atypes[0] != NCL_float) && (atypes[0] != NCL_double)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"css2c: all arguments "
                "must be all float or all double\n");
      return(NhlFATAL);
  }
  for (i = 1; i < 5; i++) {
    if (atypes[i] != atypes[0]) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"css2c: all arguments "
              "must be all float or all double\n");
    return(NhlFATAL);
    }
  }

/*
 *  Calculate the Cartesian coordinates one at a time,
 *  checking for missing values along the way.
 */
  for (i = 0; i < nt; i++) {
    if (atypes[0] == NCL_float) {
      if ( has_missing[0] && 
             (*(((float *) (datav[0]))+i) == missing[0].floatval) ) {
          *(((float *) (datav[2]))+i) = missing[0].floatval;
          *(((float *) (datav[3]))+i) = missing[0].floatval;
          *(((float *) (datav[4]))+i) = missing[0].floatval;
      }
      else if ( has_missing[1] && 
             (*(((float *) (datav[1]))+i) == missing[1].floatval) ) {
          *(((float *) (datav[2]))+i) = missing[1].floatval;
          *(((float *) (datav[3]))+i) = missing[1].floatval;
          *(((float *) (datav[4]))+i) = missing[1].floatval;
      }
      else {
        c_css2c(1, ((float *) (datav[0]))+i, ((float *) (datav[1]))+i,
                   ((float *) (datav[2]))+i, ((float *) (datav[3]))+i,
                   ((float *) (datav[4]))+i );
      }
    }
    else if (atypes[0] == NCL_double) {
      if ( has_missing[0] && 
             (*(((double *) (datav[0]))+i) == missing[0].doubleval) ) {
          *(((double *) (datav[2]))+i) = missing[0].doubleval;
          *(((double *) (datav[3]))+i) = missing[0].doubleval;
          *(((double *) (datav[4]))+i) = missing[0].doubleval;
      }
      else if ( has_missing[1] && 
             (*(((double *) (datav[1]))+i) == missing[1].doubleval) ) {
          *(((double *) (datav[2]))+i) = missing[1].doubleval;
          *(((double *) (datav[3]))+i) = missing[1].doubleval;
          *(((double *) (datav[4]))+i) = missing[1].doubleval;
      }
      else {
        c_css2cd(1, ((double *) (datav[0]))+i, ((double *) (datav[1]))+i,
                    ((double *) (datav[2]))+i, ((double *) (datav[3]))+i,
                    ((double *) (datav[4]))+i );
      }
    }
  }
}

NhlErrorTypes csc2s_W(void)
{

  int ndims[5], dsizes[5][NCL_MAX_DIMENSIONS], has_missing[5];
  NclScalar missing[5];
  void *datav[5];

  int i,j,nt;
  float fdum;
  double ddum;

  NclBasicDataTypes atypes[5];  

  for (i = 0; i < 5; i++) {
    datav[i] = NclGetArgValue(i,5,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that all dimensions are the same.
 */
  for (i = 1; i < 5; i++) {
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
    for (i = 1; i < 5; i++) {
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
 *  Check that all arguments are either all float or all double.
 */
  if ((atypes[0] != NCL_float) && (atypes[0] != NCL_double)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csc2s: all arguments "
                "must be all float or all double\n");
      return(NhlFATAL);
  }
  for (i = 1; i < 5; i++) {
    if (atypes[i] != atypes[0]) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"csc2s: all arguments "
              "must be all float or all double\n");
    return(NhlFATAL);
    }
  }

/*
 *  Calculate the lat/lon coordinates one at a time,
 *  checking for missing values along the way.
 */
  for (i = 0; i < nt; i++) {
    if (atypes[0] == NCL_float) {
      if ( has_missing[0] && 
             (*(((float *) (datav[0]))+i) == missing[0].floatval) ) {
           *(((float *) (datav[3]))+i) = missing[0].floatval;
           *(((float *) (datav[4]))+i) = missing[0].floatval;
      }
      else if ( has_missing[1] && 
             (*(((float *) (datav[1]))+i) == missing[1].floatval) ) {
           *(((float *) (datav[3]))+i) = missing[1].floatval;
           *(((float *) (datav[4]))+i) = missing[1].floatval;
      }
      else if ( has_missing[2] && 
             (*(((float *) (datav[2]))+i) == missing[2].floatval) ) {
           *(((float *) (datav[3]))+i) = missing[1].floatval;
           *(((float *) (datav[4]))+i) = missing[1].floatval;
      }
      else {
        c_csc2s(1, ((float *) (datav[0]))+i, ((float *) (datav[1]))+i,
                   ((float *) (datav[2]))+i, ((float *) (datav[3]))+i,
                   ((float *) (datav[4]))+i);
      }
    }
    else if (atypes[0] == NCL_double) {
      if ( has_missing[0] && 
             (*(((double *) (datav[0]))+i) == missing[0].doubleval) ) {
           *(((double *) (datav[3]))+i) = missing[0].doubleval;
           *(((double *) (datav[4]))+i) = missing[0].doubleval;
      }
      else if ( has_missing[1] && 
             (*(((double *) (datav[1]))+i) == missing[1].doubleval) ) {
           *(((double *) (datav[3]))+i) = missing[1].doubleval;
           *(((double *) (datav[4]))+i) = missing[1].doubleval;
      }
      else if ( has_missing[2] && 
             (*(((double *) (datav[2]))+i) == missing[2].doubleval) ) {
           *(((double *) (datav[3]))+i) = missing[1].doubleval;
           *(((double *) (datav[4]))+i) = missing[1].doubleval;
      }
      else {
        c_csc2sd(1, ((double *) (datav[0]))+i, ((double *) (datav[1]))+i,
                    ((double *) (datav[2]))+i, ((double *) (datav[3]))+i,
                    ((double *) (datav[4]))+i);
      }
    }
  }
}

NhlErrorTypes cssetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpd, i;

/*
 *  List the parameter names (integer, float, double).  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"nls", "NLS", "nsg", "NSG", "isg", "ISG", "igr", "IGR"};
  char *params_f[] = {"sig", "SIG", "tol", "TOL"};
  char *params_d[] = {"dsg", "DSG", "dtl", "DTL"};

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
           2);

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

  char  *arg1, *cval;
  int   numpi, numpf, numpd, i;
  string *pvalue, *qvalue;

/*
 *  List the parameter names (integer, float, double).  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"nls", "NLS", "nsg", "NSG", "isg", "ISG", "igr", "IGR"};
  char *params_f[] = {"sig", "SIG", "tol", "TOL"};
  char *params_d[] = {"dsg", "DSG", "dtl", "DTL"};

/*
 * Input array variable
 */
  string *pname;
  int ndims_pname, dsizes_pname[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pname;
  float  *fval;
  double *dval;
  int    *ival;
  int    ret_size = 1; 

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

  int ndims[2], dsizes[2][NCL_MAX_DIMENSIONS], has_missing[2];
  NclScalar missing[2];
  void *datav[2];

  int i,j,nt,num_points,num_missing=0;
  int *trlist,dsizes_trlist[2];

  NclBasicDataTypes atypes[2];  

  for (i = 0; i < 2; i++) {
    datav[i] = NclGetArgValue(i,2,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that all dimensions are the same.
 */
  for (i = 1; i < 2; i++) {
    if (ndims[i] != ndims[0]) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
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
        NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
                  "must have the same dimension sizes\n");
        return(NhlFATAL);
      }
    }
  }

/*
 *  Check that all arguments are either all float or all double.
 */
  if ((atypes[0] != NCL_float) && (atypes[0] != NCL_double)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
                "must be all float or all double\n");
      return(NhlFATAL);
  }
  for (i = 1; i < 2; i++) {
    if (atypes[i] != atypes[0]) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"csstri: all arguments "
              "must be all float or all double\n");
    return(NhlFATAL);
    }
  }

  num_points = dsizes[0][0];

/*
 *  Check for missing values.  Each of the two arguments must 
 *  be checked separately, since it may be that _FillValue may be
 *  set for just one of them.
 *
 *  If a missing value is found, remove that input point and collapse
 *  both input arrays.
 */
  if (has_missing[0]) {
    i = 0;
    if (atypes[0] == NCL_float) {
lab0:
      if (((*(((float *) (datav[0]))+i)) == missing[0].floatval) &&
                                (i < num_points)) {
        for (j = i; j < num_points - 1; j++) {
          *((float *) (datav[0])+j) = *((float *) (datav[0])+j+1);
          *((float *) (datav[1])+j) = *((float *) (datav[1])+j+1);
        }
        num_points--;
        if (num_points < 3) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csstri: missing values in the input data have reduced the number "
            "of valid points to less than 3.\n");
          return(NhlFATAL);
        }
        num_missing++;
        goto lab0;
      }
      i++;
      if (i < num_points) goto lab0;
    }
    else if (atypes[0] == NCL_double) {
lab1:
      if (((*(((double *) (datav[0]))+i)) == missing[0].doubleval) &&
                                (i < num_points)) {
        for (j = i; j < num_points - 1; j++) {
          *((double *) (datav[0])+j) = *((double *) (datav[0])+j+1);
          *((double *) (datav[1])+j) = *((double *) (datav[1])+j+1);
        }
        num_points--;
        if (num_points < 3) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csstri: missing values in the input data have reduced the number "
            "of valid points to less than 3.\n");
          return(NhlFATAL);
        }
        num_missing++;
        goto lab1;
      }
      i++;
      if (i < num_points) goto lab1;
    }
  }
  else if (has_missing[1]) {
    i = 0;
    if (atypes[1] == NCL_float) {
      lab2:
      if (((*(((float *) (datav[1]))+i)) == missing[1].floatval) &&
                                (i < num_points)) {
        for (j = i; j < num_points - 1; j++) {
          *((float *) (datav[0])+j) = *((float *) (datav[0])+j+1);
          *((float *) (datav[1])+j) = *((float *) (datav[1])+j+1);
        }
        num_points--;
        if (num_points < 3) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csstri: missing values in the input data have reduced the number "
            "of valid points to less than 3.\n");
          return(NhlFATAL);
        }
        num_missing++;
        goto lab2;
      }
      i++;
      if (i < num_points) goto lab2;
    }
    else if (atypes[1] == NCL_double) {
      lab3:
      if (((*(((double *) (datav[1]))+i)) == missing[1].doubleval) &&
                                (i < num_points)) {
        for (j = i; j < num_points - 1; j++) {
          *((double *) (datav[0])+j) = *((double *) (datav[0])+j+1);
          *((double *) (datav[1])+j) = *((double *) (datav[1])+j+1);
        }
        num_points--;
        if (num_points < 3) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
            "csstri: missing values in the input data have reduced the number "
            "of valid points to less than 3.\n");
          return(NhlFATAL);
        }
        num_missing++;
        goto lab3;
      }
      i++;
      if (i < num_points) goto lab3;
    }
  }

/*
 *  Issue warning if missing values have been detected.
 */
  if (num_missing > 0) {
    NhlPError(NhlWARNING,NhlEUNKNOWN,
      "csstri: missing values in %d input points - those points ignored.",
      num_missing);
  }

/*
 *  Make the call to the C function.
 */
  if (atypes[0] == NCL_float) {
    trlist = c_csstri(num_points, (float *) datav[0], (float *) datav[1], 
                      &nt, &cserr);
  }
  else if (atypes[0] == NCL_double) {
    trlist = c_csstrid(num_points, (double *) datav[0], (double *) datav[1], 
                      &nt, &cserr);
  }
  if (cserr != 0) {
    sprintf(csmsg, "csstri: Error number %d.", cserr);
    NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
    return(NhlFATAL);
  }

  dsizes_trlist[0] = nt;
  dsizes_trlist[1] = 3;
  return(NclReturnValue((void *) trlist, 2, dsizes_trlist, NULL, NCL_int, 0));
	
}

NhlErrorTypes csvoro_W(void)
{

  int ndims[10], dsizes[10][NCL_MAX_DIMENSIONS], has_missing[10];
  NclScalar missing[10];
  void *datav[10];

  int i,j,nt,num_points,num_missing=0;
  int *trlist,dsizes_trlist[10],np2,nf,nca,numv;

  NclBasicDataTypes atypes[10];

  for (i = 0; i < 10; i++) {
    datav[i] = NclGetArgValue(i,10,ndims+i,&(dsizes[i][0]),missing+i,
                              has_missing+i,atypes+i,0);
  }

/*
 *  Check that the dimension of the first two arguments are the same.
 */
  if (dsizes[0][0] != dsizes[1][0]) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: first two arguments must"
              " have the same dimension sizes\n");
    return(NhlFATAL);
  }
  num_points = dsizes[0][0];

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
 *  Check that all arguments are either all float or all double.
 */
  if ((atypes[0] != NCL_float) && (atypes[0] != NCL_double)) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: all arguments "
                "must be all float or all double\n");
      return(NhlFATAL);
  }
  if (atypes[1] != atypes[0]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: all arguments "
            "must be all float or all double\n");
    return(NhlFATAL);
  }
  for (i = 4; i < 7; i++) {
    if (atypes[i] != atypes[0]) {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"csvoro: all arguments "
              "must be all float or all double\n");
    return(NhlFATAL);
    }
  }

/*
 *  Check for missing values.  Each of the first two arguments must 
 *  be checked separately, since it may be that _FillValue may be
 *  set for just one of them.
 *
 *  If a missing value is found, remove that input point and collapse
 *  both input arrays.
 */
  for (i = 0; i < num_points; i++) {
    if (has_missing[0]) {
      if (atypes[0] == NCL_float) {
        if (*((float *) (datav[0])+i) == missing[0].floatval) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "csvoro: missing values not allowed\n");
          return(NhlFATAL);
        }
      }
      else if (atypes[0] == NCL_double) {
        if (*((double *) (datav[0])+i) == missing[0].floatval) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "csvoro: missing values not allowed\n");
          return(NhlFATAL);
        }
      }
    }
    if (has_missing[1]) {
      if (atypes[1] == NCL_float) {
        if (*((float *) (datav[1])+i) == missing[1].floatval) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "csvoro: missing values not allowed\n");
          return(NhlFATAL);
        }
      }
      else if (atypes[1] == NCL_double) {
        if (*((double *) (datav[0])+i) == missing[0].floatval) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "csvoro: missing values not allowed\n");
          return(NhlFATAL);
        }
      }
    }
  }	

/*
 *  Make the call to c_csvoro.
 */
  if (atypes[0] == NCL_float) {
    c_csvoro(num_points, (float *) datav[0], (float *) datav[1],
             *((int *) datav[2]), *((int *) datav[3]), 
             (float *) datav[4], (float *) datav[5], (float *) datav[6], 
             (int *) datav[7], (int *) datav[8], (int *) datav[9], &cserr);
    if (cserr != 0) {
      sprintf(csmsg, "csvoro: Error number %d.", cserr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
      return(NhlFATAL);
    }
    else {
      return(NhlNOERROR);
    }
  }
  else if (atypes[0] == NCL_double) {
    c_csvorod(num_points, (double *) datav[0], (double *) datav[1],
             *((int *) datav[2]), *((int *) datav[3]), 
             (double *) datav[4], (double *) datav[5], (double *) datav[6], 
             (int *) datav[7], (int *) datav[8], (int *) datav[9], &cserr);
    if (cserr != 0) {
      sprintf(csmsg, "csvoro: Error number %d.", cserr);
      NhlPError(NhlFATAL, NhlEUNKNOWN, csmsg);
      return(NhlFATAL);
    }
    else {
      return(NhlNOERROR);
    }
  }
}

NhlErrorTypes cssgrid_W(void)
{

  int ndims[5], dsizes[5][NCL_MAX_DIMENSIONS], has_missing[5];
  NclScalar missing[5],missingd[5];
  void *datav[5];

  float     *fgrid,*zout;
  double    *platd,*plond,*fvald,*rlatd,*rlond,*platdt,*plondt,*fvaldt,
            *zoutd,*ztmp;
  int       nt,psize,zdim,zsize[NCL_MAX_DIMENSIONS];
  int       i,j,k,jo,ji,nxo,nyo,num_points,num_missing;

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
    num_points = dsizes[0][0]; 
    num_missing = 0;
  }

/*
 *  Coerce the input arrays to double precision, if they
 *  are not already that.
 */
  if (atypes[0] != NCL_double) {
    platd = (double *) NclMalloc(dsizes[0][0]*sizeof(double));
    if (platd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for platd\n");
      return(NhlFATAL);
    }
    _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                platd, datav[0], dsizes[0][0], NULL, NULL,
                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[0])));
  }
  else {
    platd = (double *) datav[0];
  }
  if (atypes[1] != NCL_double) {
    plond = (double *) NclMalloc(dsizes[1][0]*sizeof(double));
    if (plond == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for plond\n");
      return(NhlFATAL);
    }
    _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                plond, datav[1], dsizes[1][0], NULL, NULL,
                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[1])));
  }
  else {
    plond = (double *) datav[1];
  }

/*
 *  Calculate the total size of argument #3, the functional values and
 *  coerce to double if necessary.  
 */
  nt = 1;
  for(j = 0; j < ndims[2]; j++) {
    nt *= dsizes[2][j];
  }
  if (atypes[2] != NCL_double) {
    fvald = (double *) NclMalloc(nt*sizeof(double));
    if (fvald == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for fvald\n");
      return(NhlFATAL);
    }
    _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                fvald, datav[2], nt, NULL, NULL,
                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[2])));
  }
  else {
    fvald = (double *) datav[2];
  }

/*
 *  Coerce the output grid to double and check for missing values.
 *  Missing values are not allowed in latitude or longitude arrays.
 */
  nxo = dsizes[3][0];
  nyo = dsizes[4][0];
  if (atypes[3] != NCL_double) {
    rlatd = (double *) NclMalloc(nxo*sizeof(double));
    if (rlatd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for rlatd\n");
      return(NhlFATAL);
    }
    _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                rlatd, datav[3], nxo, NULL, NULL,
                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[3])));
  }
  else {
    rlatd = (double *) datav[3];
  }
  if (atypes[4] != NCL_double) {
    rlond = (double *) NclMalloc(dsizes[4][0]*sizeof(double));
    if (rlond == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for rlond\n");
      return(NhlFATAL);
    }
    _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                rlond, datav[4], nyo, NULL, NULL,
                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[4])));
  }
  else {
    rlond = (double *) datav[4];
  }

/*
 *  Coerce the missing values to double.
 */
  for (i = 0; i < 5; i++) {
    if (has_missing[i]) {
      _Nclcoerce( (NclTypeClass)nclTypedoubleClass,
                  missingd+i, missing+i, 1, NULL, NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(atypes[4])));
    }
  }

  if (has_missing[3]) {
    for (i = 0; i < nxo; i++) {
      if (rlatd[i] == missingd[3].doubleval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
              "cssgrid: output latitude array is not "
              "allowed to contain a missing value.");
        return(NhlFATAL);
      }
    }
  }
  if (has_missing[4]) {
    for (i = 0; i < nyo; i++) {
      if (rlond[i] == missingd[4].doubleval) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
              "cssgrid: output longitude array is not "
              "allowed to contain a missing value.");
        return(NhlFATAL);
      }
    }
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
  if (atypes[2] == NCL_double) {
    ztype = NCL_double;
    zoutd = (double *) NclMalloc(nxo*nyo*(nt/psize)*sizeof(double));
    if (zoutd == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for zoutd\n");
      return(NhlFATAL);
    }
  }
  else {
    ztype = NCL_float;
    zout =  (float *) NclMalloc(nxo*nyo*(nt/psize)*sizeof(float));
    if (zout == NULL) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
         "cssgrid: unable to allocate memory for zout\n");
      return(NhlFATAL);
    }
  }
  platdt = (double *) NclMalloc(psize*sizeof(double));
  if (platdt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for platdt\n");
    return(NhlFATAL);
  }
  plondt = (double *) NclMalloc(psize*sizeof(double));
  if (plondt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for plondt\n");
    return(NhlFATAL);
  }
  fvaldt = (double *) NclMalloc(psize*sizeof(double));
  if (fvaldt == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
       "cssgrid: unable to allocate memory for fvaldt\n");
    return(NhlFATAL);
  }

  jo = 0;
  for (ji = 0; ji < nt; ji += psize) {
    num_points = 0;
    for (i = 0; i < psize; i++) {
/*
 *  Check for missing values and discard any input point
 *  that has a missing value.
 */
      if ( (platd[i] !=  missingd[0].doubleval) &&
           (plond[i] !=  missingd[1].doubleval) &&
           (fvald[i] !=  missingd[2].doubleval) ) {
        platdt[num_points] = platd[i];
        plondt[num_points] = plond[i];
        fvaldt[num_points] = fvald[ji+i];
        num_points++;
      }
    }
    if (num_points < 3) {
      NhlPError(NhlFATAL,NhlEUNKNOWN,
        "cssgrid: missing values in the input data have reduced the number "
        "of valid points to less than 3.\n");
      return(NhlFATAL);
    }

    ztmp = c_cssgridd(num_points,platdt,plondt,fvaldt,nxo,nyo,
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
    NclFree (platd);
  }
  if ( (void *) plond != datav[1]) {
    NclFree (plond);
  }
  if ( (void *) fvald != datav[2]) {
    NclFree (fvald);
  }
  if ( (void *) rlatd != datav[3]) {
    NclFree (platd);
  }
  if ( (void *) rlond != datav[4]) {
    NclFree (platd);
  }
  NclFree(platdt);
  NclFree(plondt);
  NclFree(fvaldt);

/*
 *  Return the array.
 */
  if (ztype == NCL_double) {
    return( NclReturnValue( (void*) zoutd, zdim, zsize, NULL, NCL_double, 0) );
  }
  else if (ztype == NCL_float) {
    return( NclReturnValue( (void*) zout, zdim, zsize, NULL, NCL_float, 0) );
  }
}

#include <string.h>
#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>
#include <ncarg/ncargC.h>


/*
 *  The following is required in order to obtain an NCL object ID
 *  when you know the HLU ID.
 */
#include "NclHLUObj.h"

NhlErrorTypes wmsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"wbf", "col",
                      "WBF", "COL"};
  char *params_f[] = {"wba", "wbc", "wbd", "wbr", "wbs", "wbt",
                      "WBA", "WBC", "WBD", "WBR", "WBS", "WBT"};
  char *params_c[] = {"erf", "ERF"};

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
              "wmsetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "wmsetp: unrecognized parameter name");
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
        c_wmseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "wmsetp: The specified value for the "
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
          c_wmsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_wmsetr(arg1, (float) *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "wmsetp: The specified value for the "
             "parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((string *) pvalue));
        c_wmsetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "wmsetp: The specified value for the "
              "parameter has an invalid type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "wmsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
}

NhlErrorTypes wmgetp_W(void)
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
  char *params_i[] = {"wbf", "col",
                      "WBF", "COL"};
  char *params_f[] = {"wba", "wbc", "wbd", "wbr", "wbs", "wbt",
                      "WBA", "WBC", "WBD", "WBR", "WBS", "WBT"};
  char *params_c[] = {"erf", "ERF"};

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
              "wmgetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "wmgetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_wmgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_wmgetr(arg1, fval);
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
             "wmgetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_wmgetc(arg1, cval, 99);
      qvalue = (string *) calloc(1,sizeof(string));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "wmgetp: impossible to get this message");
  return(NhlFATAL);
}

NhlErrorTypes wmbarb_W( void )
{
  int grlist,gkswid,i;
  int *nwid,nid;

/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x,dsizes_x[1];
  float *y;
  int ndims_y,dsizes_y[1];
  float *u;
  int ndims_u,dsizes_u[1];
  float *v;
  int ndims_v,dsizes_v[1];

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,5,     NULL,     NULL, NULL,NULL,NULL,2);

  x   = (float*) NclGetArgValue(1,5, &ndims_x, dsizes_x, NULL,NULL,NULL,2);
  y   = (float*) NclGetArgValue(2,5, &ndims_y, dsizes_y, NULL,NULL,NULL,2);
  u   = (float*) NclGetArgValue(3,5, &ndims_u, dsizes_u, NULL,NULL,NULL,2);
  v   = (float*) NclGetArgValue(4,5, &ndims_v, dsizes_v, NULL,NULL,NULL,2);
/*
 * Check the input dimension sizes.
 */
  if( ndims_x != 1 || ndims_y != 1 || ndims_u != 1 || ndims_v != 1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmbarb: input arguments must be singly-dimensioned");
        return(NhlFATAL);
  }
/*
 * Check the input sizes.
 */
  if (dsizes_x[0] != dsizes_y[0] || dsizes_y[0] != dsizes_u[0] || 
      dsizes_u[0] != dsizes_v[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmbarb: input arguments must all have the same array size");
        return(NhlFATAL);
  }

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
  NhlGetValues(nid,grlist);

/*
 * The following section calls the c_wmbarb function.
 */
  gactivate_ws (gkswid);
  for (i = 0; i < dsizes_x[0]; i++) {
    c_wmbarb(*(x+i), *(y+i), *(u+i), *(v+i));
  }
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

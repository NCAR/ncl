#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include "wrapper.h"

#define NINT(x) ( (ceil((x))-(x)) > ( (x)-floor((x)))) ? floor((x)) : ceil((x))

NhlErrorTypes wmfndn(float, float, float, float, float *, float *);

NhlErrorTypes wmsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i, j;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"alo", "aoc", "asc", "awc", "cbc", "cc1", 
                      "cc2", "cc3", "cfc", "col", "dbc", "dtc",
                      "hib", "hic", "hif", "his", "lc1", "lc2",
                      "lc3", "lob", "lof", "los", "mxs", "nbz",
                      "nms", "pai", "rbs", "rc1", "rc2", "rc3",
                      "rc4", "rc5", "rev", "rfc", "rls", "ros",
                      "sc1", "sc2", "sc3", "sc4", "slf", "sty",
                      "t1c", "t2c", "wbf", "wfc", "wty", "ezf",
                      "smf", "loc", "wdf", "unt", "vcc", "vlb",
                      "vlf", "vvc",
                      "ALO", "AOC", "ASC", "AWC", "CBC", "CC1", 
                      "CC2", "CC3", "CFC", "COL", "DBC", "DTC",
                      "HIB", "HIC", "HIF", "HIS", "LC1", "LC2",
                      "LC3", "LOB", "LOF", "LOS", "MXS", "NBZ",
                      "NMS", "PAI", "RBS", "RC1", "RC2", "RC3",
                      "RC4", "RC5", "REV", "RFC", "RLS", "ROS",
                      "SC1", "SC2", "SC3", "SC4", "SLF", "STY",
                      "T1C", "T2C", "WBF", "WFC", "WTY", "EZF",
                      "SMF", "LOC", "WDF", "UNT", "VCC", "VLB",
                      "VLF", "VVC"
                     };

  char *params_f[] = {"arc", "ard", "arl", "ars", "beg", "bet",
                      "cht", "cmg", "cs1", "cs2", "dts", "dwd",
                      "end", "lin", "lwd", "oer", "rht", "rmg",
                      "sht", "sig", "sl1", "sl2", "smt", "swi",
                      "tht", "wba", "wbc", "wbd", "wbl", "wbr",
                      "wbs", "wbt", "wht", "blw", "vrs", "vrn",
                      "vch", "vcd", "vcw", "vva",
                      "ARC", "ARD", "ARL", "ARS", "BEG", "BET",
                      "CHT", "CMG", "CS1", "CS2", "DTS", "DWD",
                      "END", "LIN", "LWD", "OER", "RHT", "RMG",
                      "SHT", "SIG", "SL1", "SL2", "SMT", "SWI",
                      "THT", "WBA", "WBC", "WBD", "WBL", "WBR",
                      "WBS", "WBT", "WHT", "BLW", "VRS", "VRN",
                      "VCH", "VCD", "VCW", "VVA"
                     };

  char *params_c[] = {"erf", "fro",
                      "ERF", "FRO",
                     };

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
           DONT_CARE);

/*
 *  Process the parameter if it has an integer value.
 */
  if (type_pvalue == NCL_int) {
    for (i = 0; i < numpi; i++) {
      if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
        if (!strncmp(arg1, "pai", 3)) {
          j = (*((int *) pvalue)) + 1;
        }
        else {
          j = *((int *) pvalue);
        }
        c_wmseti(arg1, j);
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
        cval = NrmQuarkToString( *((NrmQuark *) pvalue));
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
 *  Get values for wmap parameters.
 */

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;
  NrmQuark *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"alo", "aoc", "asc", "awc", "cbc", "cc1", 
                      "cc2", "cc3", "cfc", "col", "dbc", "dtc",
                      "hib", "hic", "hif", "his", "lc1", "lc2",
                      "lc3", "lob", "lof", "los", "mxs", "nbz",
                      "nms", "pai", "rbs", "rc1", "rc2", "rc3",
                      "rc4", "rc5", "rev", "rfc", "rls", "ros",
                      "sc1", "sc2", "sc3", "sc4", "slf", "sty",
                      "t1c", "t2c", "wbf", "wfc", "wty", "ezf",
                      "loc", "wdf",
                      "ALO", "AOC", "ASC", "AWC", "CBC", "CC1", 
                      "CC2", "CC3", "CFC", "COL", "DBC", "DTC",
                      "HIB", "HIC", "HIF", "HIS", "LC1", "LC2",
                      "LC3", "LOB", "LOF", "LOS", "MXS", "NBZ",
                      "NMS", "PAI", "RBS", "RC1", "RC2", "RC3",
                      "RC4", "RC5", "REV", "RFC", "RLS", "ROS",
                      "SC1", "SC2", "SC3", "SC4", "SLF", "STY",
                      "T1C", "T2C", "WBF", "WFC", "WTY", "EZF",
                      "LOC", "WDF"
                     };

  char *params_f[] = {"arc", "ard", "arl", "ars", "beg", "bet",
                      "cht", "cmg", "cs1", "cs2", "dts", "dwd",
                      "end", "lin", "lwd", "rht", "rmg", "sht",
                      "sl1", "sl2", "swi", "tht", "wba", "wbc",
                      "wbd", "wbl", "wbr", "wbs", "wbt", "wht",
                      "blw",
                      "ARC", "ARD", "ARL", "ARS", "BEG", "BET",
                      "CHT", "CMG", "CS1", "CS2", "DTS", "DWD",
                      "END", "LIN", "LWD", "RHT", "RMG", "SHT",
                      "SL1", "SL2", "SWI", "THT", "WBA", "WBC",
                      "WBD", "WBL", "WBR", "WBS", "WBT", "WHT",
                      "BLW"
                     };

  char *params_c[] = {"erf", "fro",
                      "ERF", "FRO",
                     };

/*
 * Input array variable
 */
  NrmQuark *pname;
  int ndims_pname;
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
      qvalue = (NrmQuark *) calloc(1,sizeof(NrmQuark));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "wmgetp: impossible to get this message");
  return(NhlFATAL);
}

NhlErrorTypes wmbarb_W( void )
{
  ng_size_t i;
  int grlist,gkswid,btot;
  int *nwid,nid,ezf;
  float xt,yt,xtn,ytn,ang1,ang2,utmp,vtmp,vlen,d2r=0.01745329;
 
  int itrn;
  float x1,x2,y1,y2,xx1,xx2,yy1,yy2;

/*
 *  Define a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *v;
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  int has_missing_v;

  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv;
  NclScalar missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;

  float u_missing, v_missing;

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,5,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,5, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,5, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  u   = (float*) NclGetArgValue(3,5, &ndims_u, dsizes_u, 
            &missing_u, &has_missing_u, &type_u, DONT_CARE);
  v   = (float*) NclGetArgValue(4,5, &ndims_v, dsizes_v, 
            &missing_v, &has_missing_v, &type_v, DONT_CARE);

/*
 * Check that the input dimensions and dimension sizes are the same.
 */
  if( ndims_x != ndims_y || ndims_y != ndims_u || ndims_u != ndims_v) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmbarb: input arguments must have the same dimensions.");
        return(NhlFATAL);
  }
/*
 * Check the dimension sizes.
 */
  for (i = 0; i < ndims_x; i++) {
    if (dsizes_x[i] != dsizes_y[i] || dsizes_y[i] != dsizes_u[i] ||
        dsizes_u[i] != dsizes_v[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "wmbarb: input arguments must all have the same dimension sizes");
          return(NhlFATAL);
    }
  }


/*
 * Coerce missing values to float.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
  u_missing = missing_ru.floatval;
  v_missing = missing_rv.floatval;

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
 *  Calculate the total number of wind barbs to draw.
 */
  btot = 1;
  for (i = 0; i < ndims_x; i++) {
    btot = btot*dsizes_x[i];
  }

/*
 * The following section calls the c_wmbarb function.
 */
  gactivate_ws (gkswid);
  c_wmgeti("ezf",&ezf);
  if (ezf != -1) {
    for (i = 0; i < btot; i++) {
      if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
        c_getset(&x1,&x2,&y1,&y2,&xx1,&xx2,&yy1,&yy2,&itrn);
/*
 * Find a small vector *on the map* in the direction of the wind barb.
 * The cos term is introduced to accommodate for the latitude of the
 * barb - as you approach the poles, a given spacial distance in latitude 
 * in degrees is less than the same spacial distance in degrees
 * longitude.
 */
        ang1 = atan2(*(u+i),*(v+i));
        c_maptrn(*(x+i), *(y+i), &xt, &yt);
        if (xt != 1.e12) {
          c_maptrn(*(x+i)+0.1*cos(ang1),*(y+i)+0.1*sin(ang1)/cos(*(x+i)*d2r),&xtn,&ytn);
          ang2 = atan2(ytn-yt,xtn-xt);
          utmp = *(u+i);
          vtmp = *(v+i);
          vlen = sqrt(utmp*utmp + vtmp*vtmp);
          utmp = vlen*cos(ang2);
          vtmp = vlen*sin(ang2);
          c_wmbarb(xt, yt, utmp,vtmp);
        }
      }
    }
  }
  else {
    for (i = 0; i < btot; i++) {
      if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
          c_wmbarb(*(x+i), *(y+i), *(u+i), *(v+i));
      }
    }
  }
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

NhlErrorTypes wmbarbmap_W( void )
{
  ng_size_t i;
  int grlist,gkswid,btot;
  int *nwid,nid,ezf,wdf;
  float xt,yt,xtn,ytn,ang1,ang2,utmp,vtmp,vlen,d2r=0.01745329;

/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *v;
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  int has_missing_v;

  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv;
  NclScalar missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;

  float u_missing, v_missing;

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,5,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,5, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,5, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  u   = (float*) NclGetArgValue(3,5, &ndims_u, dsizes_u, 
            &missing_u, &has_missing_u, &type_u, DONT_CARE);
  v   = (float*) NclGetArgValue(4,5, &ndims_v, dsizes_v, 
            &missing_v, &has_missing_v, &type_v, DONT_CARE);

/*
 * Coerce missing values to float.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
  u_missing = missing_ru.floatval;
  v_missing = missing_rv.floatval;

/*
 * Check that the input dimensions and dimension sizes are the same.
 */
  if( ndims_x != ndims_y || ndims_y != ndims_u || ndims_u != ndims_v) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmbarbmap: input arguments must have the same dimensions.");
        return(NhlFATAL);
  }
/*
 * Check the dimension sizes.
 */
  for (i = 0; i < ndims_x; i++) {
    if (dsizes_x[i] != dsizes_y[i] || dsizes_y[i] != dsizes_u[i] || 
        dsizes_u[i] != dsizes_v[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "wmbarbmap: input arguments must all have the same dimension sizes");
          return(NhlFATAL);
    }
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

/*
 *  Save the current setting for ezf and set it to "1" to indicate
 *  drawing wind barbs over a map.  Also save the setting for WDF and
 *  set it to "1" to indicate using the meteorological convention
 *  for wind barb directions.  These values will be restored
 *  before returning.
 */
  c_wmgeti("ezf",&ezf);
  c_wmgeti("wdf",&wdf);
  c_wmseti("ezf",1);
  c_wmseti("wdf",1);

/*
 *  Calculate the total number of wind barbs to draw.
 */
  btot = 1;
  for (i = 0; i < ndims_x; i++) {
    btot = btot*dsizes_x[i];
  }

/*
 *  Draw the wind barbs.
 */
  for (i = 0; i < btot; i++) {
/*
 * Find a small vector *on the map* in the direction of the wind barb.
 * The cos term is introduced to accommodate for the latitude of the
 * barb - as you approach the poles, a given spacial distance in latitude 
 * in degrees is less than the same spacial distance in degrees
 * longitude.
 */
    if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
      ang1 = atan2(*(u+i),*(v+i));
      c_maptrn(*(x+i), *(y+i), &xt, &yt);
      if (xt != 1.e12) {
        c_maptrn(*(x+i)+0.1*cos(ang1),*(y+i)+0.1*sin(ang1)/cos(*(x+i)*d2r),&xtn,&ytn);
        ang2 = atan2(ytn-yt,xtn-xt);
        utmp = *(u+i);
        vtmp = *(v+i);
        vlen = sqrt(utmp*utmp + vtmp*vtmp);
        utmp = vlen*cos(ang2);
        vtmp = vlen*sin(ang2);
        c_wmbarb(xt, yt, utmp,vtmp);
      }
    }
  }

/*
 *  Restore the settings of ezf and wdf.
 */
  c_wmseti("ezf",ezf);
  c_wmseti("wdf",wdf);

  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

NhlErrorTypes wmvect_W( void )
{
  ng_size_t i;
  int grlist,gkswid,btot;
  int *nwid,nid,ezf;
  float xt,yt,xtn,ytn,ang1,ang2,utmp,vtmp,vlen,d2r=0.01745329;
 
  int itrn;
  float x1,x2,y1,y2,xx1,xx2,yy1,yy2;

/*
 *  Define a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *v;
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  int has_missing_v;

  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv;
  NclScalar missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;

  float u_missing, v_missing;

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,5,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,5, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,5, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  u   = (float*) NclGetArgValue(3,5, &ndims_u, dsizes_u, 
            &missing_u, &has_missing_u, &type_u, DONT_CARE);
  v   = (float*) NclGetArgValue(4,5, &ndims_v, dsizes_v, 
            &missing_v, &has_missing_v, &type_v, DONT_CARE);

/*
 * Check that the input dimensions and dimension sizes are the same.
 */
  if( ndims_x != ndims_y || ndims_y != ndims_u || ndims_u != ndims_v) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmvect: input arguments must have the same dimensions.");
        return(NhlFATAL);
  }
/*
 * Check the dimension sizes.
 */
  for (i = 0; i < ndims_x; i++) {
    if (dsizes_x[i] != dsizes_y[i] || dsizes_y[i] != dsizes_u[i] ||
        dsizes_u[i] != dsizes_v[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "wmvect: input arguments must all have the same dimension sizes");
          return(NhlFATAL);
    }
  }


/*
 * Coerce missing values to float.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
  u_missing = missing_ru.floatval;
  v_missing = missing_rv.floatval;

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
 *  Calculate the total number of vectors to draw.
 */
  btot = 1;
  for (i = 0; i < ndims_x; i++) {
    btot = btot*dsizes_x[i];
  }

/*
 * The following section calls the c_wmvect function.
 */
  gactivate_ws (gkswid);
  c_wmgeti("ezf",&ezf);
  if (ezf != -1) {
    for (i = 0; i < btot; i++) {
      if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
        c_getset(&x1,&x2,&y1,&y2,&xx1,&xx2,&yy1,&yy2,&itrn);
/*
 * Find a small vector *on the map* in the direction of the vector.
 * The cos term is introduced to accommodate for the latitude of the
 * vector - as you approach the poles, a given spacial distance in latitude 
 * in degrees is less than the same spacial distance in degrees
 * longitude.
 */
        ang1 = atan2(*(u+i),*(v+i));
        c_maptrn(*(x+i), *(y+i), &xt, &yt);
        if (xt != 1.e12) {
          c_maptrn(*(x+i)+0.1*cos(ang1),*(y+i)+0.1*sin(ang1)/cos(*(x+i)*d2r),&xtn,&ytn);
          ang2 = atan2(ytn-yt,xtn-xt);
          utmp = *(u+i);
          vtmp = *(v+i);
          vlen = sqrt(utmp*utmp + vtmp*vtmp);
          utmp = vlen*cos(ang2);
          vtmp = vlen*sin(ang2);
          c_wmvect(xt, yt, utmp,vtmp);
        }
      }
    }
  }
  else {
    for (i = 0; i < btot; i++) {
      if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
          c_wmvect(*(x+i), *(y+i), *(u+i), *(v+i));
      }
    }
  }
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

NhlErrorTypes wmvectmap_W( void )
{
  ng_size_t i;
  int grlist,gkswid,btot;
  int *nwid,nid;
 
/*
 *  Define a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];
  float *u;
  int ndims_u;
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  int has_missing_u;
  float *v;
  int ndims_v;
  ng_size_t dsizes_v[NCL_MAX_DIMENSIONS];
  int has_missing_v;

  NclScalar missing_u, missing_v;
  NclScalar missing_du, missing_dv;
  NclScalar missing_ru, missing_rv;
  NclBasicDataTypes type_u, type_v;

  float u_missing, v_missing;

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,5,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,5, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,5, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  u   = (float*) NclGetArgValue(3,5, &ndims_u, dsizes_u, 
            &missing_u, &has_missing_u, &type_u, DONT_CARE);
  v   = (float*) NclGetArgValue(4,5, &ndims_v, dsizes_v, 
            &missing_v, &has_missing_v, &type_v, DONT_CARE);

/*
 * Check that the input dimensions and dimension sizes are the same.
 */
  if( ndims_x != ndims_y || ndims_y != ndims_u || ndims_u != ndims_v) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmvectmap: input arguments must have the same dimensions.");
        return(NhlFATAL);
  }
/*
 * Check the dimension sizes.
 */
  for (i = 0; i < ndims_x; i++) {
    if (dsizes_x[i] != dsizes_y[i] || dsizes_y[i] != dsizes_u[i] ||
        dsizes_u[i] != dsizes_v[i]) {
          NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "wmvectmap: input arguments must all have the same dimension sizes");
          return(NhlFATAL);
    }
  }


/*
 * Coerce missing values to float.
 */
  coerce_missing(type_u,has_missing_u,&missing_u,&missing_du,&missing_ru);
  coerce_missing(type_v,has_missing_v,&missing_v,&missing_dv,&missing_rv);
  u_missing = missing_ru.floatval;
  v_missing = missing_rv.floatval;

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
 *  Calculate the total number of vectorsto draw.
 */
  btot = 1;
  for (i = 0; i < ndims_x; i++) {
    btot = btot*dsizes_x[i];
  }

/*
 * The following section calls the c_wmvectmap function.
 */
  gactivate_ws (gkswid);
  for (i = 0; i < btot; i++) {
    if ( (*(u+i) != u_missing) && (*(v+i) != v_missing)) {
        c_wmvectmap(*(x+i), *(y+i), *(u+i), *(v+i));
    }
  }
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

NhlErrorTypes wmvlbl_W( void )
{
  ng_size_t i;
  int grlist,gkswid;
  int *nwid,nid;
 
/*
 *  Define a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[NCL_MAX_DIMENSIONS];

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,3,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,3, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,3, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);

/*
 * Check that the input dimensions and dimension sizes are the same.
 */
  if( ndims_x != ndims_y) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmvectmap: input arguments must have the same dimensions.");
        return(NhlFATAL);
  }
/*
 * Check the dimension sizes.
 */
  for (i = 0; i < ndims_x; i++) {
    if (dsizes_x[i] != dsizes_y[i]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
          "wmvlbl: input arguments must all have the same dimension sizes");
          return(NhlFATAL);
    }
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
 * The following section calls the c_wmvlbl function.
 */
  gactivate_ws (gkswid);
  c_wmvlbl(*x, *y);
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}

NhlErrorTypes wmdrft_W( void )
{
  int ier;
  Gclip clip_ind_rect;

  ng_size_t i;
  int grlist,gkswid;
  int *nwid,nid,ezf,j,indx,numi;
  float *xd,*yd,*ud,*vd,xt,yt,mval=1.e12;

/*
 *  Define a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[1];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[1];

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,3,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,3, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,3, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);
/*
 * Check the input dimension sizes.
 */
  if( ndims_x != 1 || ndims_y != 1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmdrft: input arguments must be singly-dimensioned");
        return(NhlFATAL);
  }
/*
 * Check the input sizes.
 */
  if (dsizes_x[0] != dsizes_y[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmdrft: input arguments must all have the same array size");
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
 * The following section calls the c_wmdrft function.
 */
  gactivate_ws (gkswid);
  ginq_clip(&ier,&clip_ind_rect);
  gset_clip_ind(GIND_CLIP);
  
/*
 *  If wmdrft is being used in conjunction with a 
 *  currently existing map projection, then the
 *  input is assumed to be in degrees and it must
 *  be determined what parts of the front line are
 *  projectable.  If one point is projectable and
 *  its neighbor is not, then you need to figure
 *  out where between those two points the closest
 *  projectable point is.
 */
  c_wmgeti("ezf",&ezf);
  if (ezf != -1) {
    numi = dsizes_x[0];
    xd = (float *) malloc(numi*sizeof(float));
    yd = (float *) malloc(numi*sizeof(float));
    ud = (float *) calloc(numi,sizeof(float));
    vd = (float *) calloc(numi,sizeof(float));
    xd = (float *) memcpy(xd,x,numi*sizeof(float));
    yd = (float *) memcpy(yd,y,numi*sizeof(float));
  
    for (i = 0; i < numi; i++) {
      if (i == numi-1) {
        free(xd);
        free(yd);
        free(ud);
        free(vd);
        gdeactivate_ws (gkswid);
        NhlRLDestroy(grlist);
        return(NhlNOERROR);
      }
      c_maptrn(xd[i],yd[i],&xt,&yt);
      if (xt != mval) {
        indx = 0;
        ud[0] = xt;
        vd[0] = yt;
        for (j = i+1; j < numi; j++) {
          indx++;
          c_maptrn(xd[j],yd[j],&xt,&yt);
          if (xt != mval) {
            ud[indx] = xt;
            vd[indx] = yt;
            if (j == numi-1) {
              c_wmdrft(indx+1,ud,vd);
              free(xd);
              free(yd);
              free(ud);
              free(vd);
              gdeactivate_ws (gkswid);
              NhlRLDestroy(grlist);
              return(NhlNOERROR);
            }
          }
          else {
            wmfndn(xd[j-1],yd[j-1],xd[j],yd[j],&xt,&yt);
            c_maptrn(xt,yt,ud+indx,vd+indx);
            c_wmdrft(indx+1,ud,vd);
            i = i+indx;
            j = numi;
            indx = 0;
          }
        }
      }
      else {
        indx = 0;
        for (j = i+1; j < numi; j++) {
          indx++;
          if (j == numi-1) {
            free(xd);
            free(yd);
            free(ud);
            free(vd);
            gdeactivate_ws (gkswid);
            NhlRLDestroy(grlist);
            return(NhlNOERROR);
          }
          c_maptrn(xd[j],yd[j],&xt,&yt);
          if (xt != mval) {
            wmfndn(xd[j-1],yd[j-1],xd[j],yd[j],&xt,&yt);
            xd[j-1] = xt;
            yd[j-1] = yt;
            i = j-2;
            j = numi;
          }
        }
      }
    }
  }
  else { 
    c_wmdrft(dsizes_x[0], x, y);
  }

  gset_clip_ind(clip_ind_rect.clip_ind);
  gdeactivate_ws (gkswid);
  NhlRLDestroy(grlist);
  return(NhlNOERROR);
  
}

/*
 *  wmfndn takes two input lat/lon coordinate pairs (expressed in
 *  degrees) where one of the points produces an invalid projection
 *  point and the other produces a valid projection point.  The function
 *  returns a point in between the two input points that represents where
 *  the changeover from valid projection point to invalid occurs.
 *  The returned point produces a valid projection point.  
 */
NhlErrorTypes wmfndn(float x1, float y1, float x2, float y2, 
                     float *xp, float *yp)
{
/*
 *  Specify the flag value indicating an invalid projection point.
 */
  float mval=1.e12;
/*
 *  Specify the tolerance for convergence.
 */
  float eps=0.000001;
  
  float xh,yh,xho,yho,x1t,y1t,x2t,y2t,xht,yht,u1,v1,u2,v2;

  c_maptrn(x1,y1,&u1,&v1);
  c_maptrn(x2,y2,&u2,&v2);
  if ( (u1 == mval) && (u2 != mval)) {
    x1t = x2;
    y1t = y2; 
    x2t = x1;
    y2t = y1; 
  }
  else if ( (u1 != mval) && (u2 == mval)) {
    x1t = x1;
    y1t = y1;
    x2t = x2;
    y2t = y2; 
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "wmfndn - arguments must produce one valid projection point \n"
        "         and one invalid projection point\n");
    return(NhlFATAL);
  }
  xh  = 0.5*(x1t+x2t);
  yh  = 0.5*(y1t+y2t);

  do {
    c_maptrn(xh,yh,&xht,&yht);
    if (xht != mval) {
      x1t = xh;
      y1t = yh;
    }
    else {
      x2t = xh;
      y2t = yh;
    }
    xho = xh;
    yho = yh;
    xh = 0.5*(x1t+x2t);
    yh = 0.5*(y1t+y2t);
  } while ( ((xho-xh)*(xho-xh) + (yho-yh)*(xho-xh)) > eps);

  *xp = x1t;
  *yp = y1t;

  return(NhlNOERROR);

}

NhlErrorTypes wmlabs_W( void )
{
  int ier;
  Gclip clip_ind_rect;

  int grlist,gkswid,i;
  int *nwid,nid,ezf;
  char *arg1;
  float xt,yt;

/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[1];
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[1];
  NrmQuark *symtyp;
  int ndims_symtyp;
  ng_size_t dsizes_symtyp[NCL_MAX_DIMENSIONS];
  

/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,4,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,4, &ndims_x, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y   = (float*) NclGetArgValue(2,4, &ndims_y, dsizes_y, NULL,NULL,NULL,DONT_CARE);

/*
 * Check the input dimension sizes.
 */
  if( ndims_x != 1 || ndims_y != 1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmlabs: input arguments must be singly-dimensioned");
        return(NhlFATAL);
  }
/*
 * Check the input sizes.
 */
  if (dsizes_x[0] != dsizes_y[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmlabs: input arguments must all have the same array size");
        return(NhlFATAL);
  }

/*
 * Retrieve the symbol type.
 */
  symtyp = (NrmQuark *) NclGetArgValue(
          3,
          4,
          &ndims_symtyp,
          dsizes_symtyp,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

/*
 * Check number of dimensions for the symbol type.
 */
  if(ndims_symtyp != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "wmlabs: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }
  arg1 = NrmQuarkToString(*symtyp);


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
 * The following section calls the c_wmwmlabs function.
 */
  gactivate_ws (gkswid);
  ginq_clip(&ier,&clip_ind_rect);
  gset_clip_ind(GIND_CLIP);
  c_wmgeti("ezf",&ezf);
  if (ezf != -1) {
    for (i = 0; i < dsizes_x[0]; i++) {
      c_maptrn(x[i],y[i],&xt,&yt);
      if (xt != 1.e12) {
        c_wmlabs(xt, yt, arg1);
      }
    }
  }
  else {
    for (i = 0; i < dsizes_x[0]; i++) {
      c_wmlabs(*(x+i), *(y+i), arg1);
    }
  }
  gset_clip_ind(clip_ind_rect.clip_ind);
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}


NhlErrorTypes wmstnm_W( void )
{
  int ier;
  Gclip clip_ind_rect;

  ng_size_t i;
  int grlist,gkswid,num_strings;
  int *nwid,nid,ezf,iang;
  char *arg1,tang[3],*tsym;
  float xt,yt,xtt,ytt,fang;

/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *x;
  int ndims_x;
  ng_size_t dsizes_x[1];
  int has_missing_x;
  float *y;
  int ndims_y;
  ng_size_t dsizes_y[1];
  int has_missing_y;
  NrmQuark *symtyp;
  int ndims_symtyp;
  ng_size_t dsizes_symtyp[NCL_MAX_DIMENSIONS];

  NclScalar missing_x, missing_y;
  NclScalar missing_dx, missing_dy;
  NclScalar missing_rx, missing_ry;
  NclBasicDataTypes type_x, type_y;

  float x_missing, y_missing;
  
/*
 * Retrieve parameters
 */

/*
 *  nwid points to the HLU identifier of the graphic object; this is
 *  converted to the NCL workstation identifier below.
 */
  nwid = (int*)  NclGetArgValue(0,4,     NULL,     NULL, NULL,NULL,NULL,DONT_CARE);

  x   = (float*) NclGetArgValue(1,4, &ndims_x, dsizes_x,
                         &missing_x, &has_missing_x, &type_x, DONT_CARE);
  y   = (float*) NclGetArgValue(2,4, &ndims_y, dsizes_y,
                         &missing_y, &has_missing_y, &type_y, DONT_CARE);

/*
 * Check the input dimension sizes.
 */
  if( ndims_x != 1 || ndims_y != 1) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmstnm: input arguments must be singly-dimensioned");
        return(NhlFATAL);
  }
/*
 * Check the input sizes.
 */
  if (dsizes_x[0] != dsizes_y[0]) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
               "wmstnm: input arguments must be the same size");
        return(NhlFATAL);
  }

/*
 * Retrieve the station model data.
 */
  symtyp = (NrmQuark *) NclGetArgValue(
          3,
          4,
          &ndims_symtyp,
          dsizes_symtyp,
          NULL,
          NULL,
          NULL,
          DONT_CARE);


/*
 * Check number of dimensions for the model data.
 */
  if(ndims_symtyp != 1) {
    NhlPError(NhlFATAL, NhlEUNKNOWN,
              "wmstnm: Argument #4 has the wrong number of dimensions.");
    return(NhlFATAL);
  }


/*
 * Coerce missing values to float.
 */
  coerce_missing(type_x,has_missing_x,&missing_x,&missing_dx,&missing_rx);
  coerce_missing(type_y,has_missing_y,&missing_y,&missing_dy,&missing_ry);
  x_missing = missing_rx.floatval;
  y_missing = missing_ry.floatval;

  arg1 = NrmQuarkToString(*symtyp);
/*
 *  Calculate the number of strings (this is just a product of
 *  the dimension sizes over the number of dimensions).
 */
  if(ndims_symtyp > 1) {
    num_strings = 1;     
    for (i = 0; i < ndims_symtyp; i++ ) {
      num_strings *= dsizes_symtyp[i];
    }
  }
  else {
    num_strings = dsizes_symtyp[0];
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
 * The following section calls the c_wmstnm function.
 */
  gactivate_ws (gkswid);
  ginq_clip(&ier,&clip_ind_rect);
  gset_clip_ind(GIND_CLIP);

  for (i = 0; i < num_strings; i++) {

    c_wmgeti("ezf",&ezf);
    if (ezf != -1) {
      arg1 = NrmQuarkToString(symtyp[i]);
      tsym = (char *)calloc(strlen(arg1)+1,sizeof(char));
      strcpy(tsym,arg1);
      c_maptrn(x[i],y[i],&xt,&yt);
      if (xt != 1.e12) {
        tang[0] = tsym[6];
        tang[1] = tsym[7];
        tang[2] = 0;
        fang = 10.*0.0174532925199*atof(tang); 
        c_maptrn(x[i]+0.1*cos(fang), 
             y[i]+0.1/cos(0.0174532925199*x[i])*sin(fang), &xtt, &ytt);
        fang = fmod(57.2957795130823*atan2(xtt-xt, ytt-yt)+360., 360.);
        iang = (int) max(0,min(35,NINT(fang/10.)));
        sprintf(tang,"%2d",iang);
        tsym[6] = tang[0];
        tsym[7] = tang[1];
        c_wmstnm(xt, yt, tsym);
        free(tsym);
      }
    }
    else {
      for (i = 0; i < dsizes_x[0]; i++) {
        c_wmstnm(*(x+i), *(y+i), arg1);
      }
    }
  }

  gset_clip_ind(clip_ind_rect.clip_ind);
  gdeactivate_ws (gkswid);

  NhlRLDestroy(grlist);

  return(NhlNOERROR);
  
}


#include <string.h>
#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
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

NhlErrorTypes tdinit_W( void )
{
/*
 * Input variables
 */
  float *mid, *orig, *third, *otep;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  mid = (float*)NclGetArgValue(
          0,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  orig = (float*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  third = (float*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  otep = (float*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  c_tdinit(mid[0],mid[1],mid[2],orig[0],orig[1],orig[2],
		   third[0],third[1],third[2],*otep);

  return(NhlNOERROR);
}


NhlErrorTypes tdpara_W( void )
{
/*
 * Input variables
 */
  float *a00, *a10, *a01;

/*
 * Retrieve parameters
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  a00 = (float*)NclGetArgValue(
          0,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  a10 = (float*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  a01 = (float*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          2);

  c_tdpara(a00[0],a00[1],a00[2],a10[0],a10[1],a10[2],a01[0],a01[1],a01[2]);

  return(NhlNOERROR);
}


NhlErrorTypes tdez2d_W( void )
{
  int grlist,i,j;
  float *zp;

/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  int ier = 0;
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS];
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS];
  float *s1,*s2,*s3;
  int gkswid,*style,*nwid,nid;
  int *iwk;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid   = (int*)NclGetArgValue(0,8,NULL,NULL,NULL,NULL,NULL,2);
  x      = (float*)NclGetArgValue(1,8, &ndims_x, dsizes_x, NULL,NULL,NULL,2);
  y      = (float*)NclGetArgValue(2,8, &ndims_y, dsizes_y, NULL,NULL,NULL,2);
  z      = (float*)NclGetArgValue(3,8, &ndims_z, dsizes_z, NULL,NULL,NULL,2 );
  s1     = (float*)NclGetArgValue(4,8,NULL,NULL,NULL,NULL,NULL,2);
  s2     = (float*)NclGetArgValue(5,8,NULL,NULL,NULL,NULL,NULL,2);
  s3     = (float*)NclGetArgValue(6,8,NULL,NULL,NULL,NULL,NULL,2);
  style  = (int*)NclGetArgValue(7,8,NULL,NULL,NULL,NULL,NULL,2);
/*
 * Check the input sizes.
 */
  if( ndims_x != 1 || ndims_y != 1 || ndims_z != 2) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez2d: x and y must be one dimension and z must be 2 dimensions");
	return(NhlFATAL);
  }

  if( (dsizes_x[0] == dsizes_z[0]) && (dsizes_y[0] == dsizes_z[1]) ) {
/*
 * Reverse the order of the dimensions.
 */
  zp = (float *) calloc(dsizes_x[0] * dsizes_y[0],sizeof(float));
  for (i = 0; i < dsizes_x[0]; i++) {
    for (j = 0; j < dsizes_y[0]; j++) { 
      zp[j*dsizes_x[0] + i] = z[i*dsizes_y[0] + j];
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
 * The following section calls the tdez2d function.
 */
        gactivate_ws (gkswid);
        c_tdez2d(dsizes_x[0],dsizes_y[0],x,y,zp,*s1,*s2,*s3,*style);
        gdeactivate_ws (gkswid);
        free(zp);
  }
  else { NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez2d: the dimension sizes of z must be the dimension of x by the dimension of y");
        return(NhlFATAL);
  }

   return(NhlNOERROR);
  
}

NhlErrorTypes tdez3d_W( void )
{
/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  float *up;
  int ier = 0;
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS];
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS];
  float *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS];
  float *value;
  float *s1,*s2,*s3;
  int gkswid,grlist,*nwid,nid,*style;
  int i,j,k;
  int *iwk;

/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid =  (int*)NclGetArgValue(0,10,NULL,NULL,NULL,NULL,NULL,2);
  x =     (float*)NclGetArgValue(1,10, &ndims_x, dsizes_x, NULL,NULL,NULL,2);
  y =     (float*)NclGetArgValue(2,10, &ndims_y, dsizes_y, NULL,NULL,NULL,2);
  z =     (float*)NclGetArgValue(3,10, &ndims_z, dsizes_z, NULL,NULL,NULL,2 );
  u =     (float*)NclGetArgValue(4,10, &ndims_u, dsizes_u, NULL,NULL,NULL,2 );
  value = (float*)NclGetArgValue(5,10,NULL,NULL,NULL,NULL,NULL,2);
  s1 =    (float*)NclGetArgValue(6,10,NULL,NULL,NULL,NULL,NULL,2);
  s2 =    (float*)NclGetArgValue(7,10,NULL,NULL,NULL,NULL,NULL,2);
  s3 =    (float*)NclGetArgValue(8,10,NULL,NULL,NULL,NULL,NULL,2);
  style = (int*)NclGetArgValue(9,10,NULL,NULL,NULL,NULL,NULL,2);
/*
 * Check input sizes.
 */
  if( ndims_x != 1 || ndims_y != 1 || ndims_z != 1 || ndims_u != 3) {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez3d: x, y, and z must be one dimension and u must be 3 dimensions");
	return(NhlFATAL);
  }
  if( (dsizes_x[0] == dsizes_u[0]) && (dsizes_y[0] == dsizes_u[1]) && (dsizes_z[0] == dsizes_u[2]) ) {
/*
 * The following section calls the tdez3d function.
 */

/*
 * Reverse the order of the dimensions.
 */
  up = (float *) calloc(dsizes_x[0] * dsizes_y[0] * dsizes_z[0],sizeof(float));
  for (i = 0; i < dsizes_x[0]; i++) {
    for (j = 0; j < dsizes_y[0]; j++) { 
      for (k = 0; k < dsizes_z[0]; k++) { 
  
        up[dsizes_x[0]*dsizes_y[0]*k + j*dsizes_x[0] + i] = 
                 u[i*dsizes_z[0]*dsizes_y[0] + dsizes_z[0]*j + k];
      }
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

         gactivate_ws (gkswid);
	 c_tdez3d(dsizes_x[0],dsizes_y[0],dsizes_z[0],x,y,z,up,*value,*s1,*s2,*s3,*style);
         gdeactivate_ws (gkswid);
         free(up);
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez3d: the dimension sizes of u must be the dimension of x by the dimension of y by the dimension of z");
	return(NhlFATAL);
  }
   return(NhlNOERROR);
  
}

/*
 * The tdsetp_W code is based on Fred Clare's wmsetp_W code.
 */

NhlErrorTypes tdsetp_W(void)
{
  char  *arg1;
  int   numpi, numpf, i, j;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"cs1", "cs2", "fov", "hnd", "ifc1", "ifc2", 
                      "ifc3", "ifc4", "ilc1", "ilc2", "iltd",
                      "lsu", "lsv", "lsw", "set", "shd", "ste", 
                      "CS1", "CS2", "FOV", "HND", "IFC1", "IFC2", 
                      "IFC3", "IFC4", "ILC1", "ILC2", "ILTD",
                      "LSU", "LSV", "LSW", "SET", "SHD", "STE", 
  };
  char *params_f[] = {"cs1", "cs2", "fov", "lsu", "lsv", "lsw", 
					  "vpb", "vpl", "vpr", "vpt", "ustp", "vstp", "wstp"
					  "CS1", "CS2", "FOV", "LSU", "LSV", "LSW", 
					  "VPB", "VPL", "VPR", "VPT", "USTP", "VSTP", "WSTP"
  };
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

  arg1 = NrmQuarkToString(*pname);
 
/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "tdsetp: unrecognized parameter name");
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
		j = *((int *) pvalue);
        c_tdseti(arg1, j);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdsetp: The specified value for the parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_tdsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_tdsetr(arg1, (float) *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdsetp: The specified value for the parameter has an invalid type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
}

NhlErrorTypes tdgetp_W(void)
{
/*
 *  Get values for tdpack parameters.
 */
  char  *arg1;
  int   numpi, numpf, i;
  string *pvalue, *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"cs1", "cs2", "fov", "hnd", "ifc1", "ifc2", 
                      "ifc3", "ifc4", "ilc1", "ilc2", "iltd",
                      "lsu", "lsv", "lsw", "set", "shd", "ste", 
                      "CS1", "CS2", "FOV", "HND", "IFC1", "IFC2", 
                      "IFC3", "IFC4", "ILC1", "ILC2", "ILTD",
                      "LSU", "LSV", "LSW", "SET", "SHD", "STE", 
  };
  char *params_f[] = {"cs1", "cs2", "fov", "lsu", "lsv", "lsw", 
					  "vpb", "vpl", "vpr", "vpt", "ustp", "vstp", "wstp"
					  "CS1", "CS2", "FOV", "LSU", "LSV", "LSW", 
					  "VPB", "VPL", "VPR", "VPT", "USTP", "VSTP", "WSTP"
  };
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

  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpi = sizeof(params_i)/sizeof(void *);
  numpf = sizeof(params_f)/sizeof(void *);
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "tdgetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_tdgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_tdgetr(arg1, fval);
      return(NclReturnValue((void *) fval, 1, &ret_size, NULL, NCL_float, 0));
    }
  }

  NhlPError(NhlFATAL, NhlEUNKNOWN, "tdgetp: impossible to get this message");
  return(NhlFATAL);
}


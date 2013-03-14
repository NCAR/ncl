#include <string.h>
#include <stdio.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include "wrapper.h"
#include <ncarg/ncargC.h>

extern void NGCALLF(tdcurv,tdcurv)(float *,float *,float*,int *,int *, float *,
				   float *);

extern void NGCALLF(tdttri,tdttri)(float *,float *,float*,int *,int *, float *,
                                   float *,float *,int *,int *,int *,float *, 
                                   float *,float *,float *,float *,float *);

extern void NGCALLF(tditri,tditri)(float *,int *,float *,int *, float *,int *,
                                   float *,int *,int *,float *,float *,int *,
                                   int *, int *);

extern void NGCALLF(tdstri,TDSTRI)(float *,int *,float *,int *,float *,int *,
                                   float *,int *,int *,int *);

extern void NGCALLF(tdotri,TDOTRI)(float *,int *,int *,float *,int *,int *);

extern void NGCALLF(tdez1d,TDEZ1D)(int *,float *,float *,float*,int *,float *,
                                   float *,float *,float *,float *,int *);

NhlErrorTypes tdinit_W( void )
{
/*
 * Input variables
 */
  float *mid, *orig, *third, *otep;

/*
 * Retrieve parameters.
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
          DONT_CARE);

  orig = (float*)NclGetArgValue(
          1,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  third = (float*)NclGetArgValue(
          2,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  otep = (float*)NclGetArgValue(
          3,
          4,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  c_tdinit(mid[0],mid[1],mid[2],orig[0],orig[1],orig[2],
           third[0],third[1],third[2],*otep);

  return(NhlNOERROR);
}


NhlErrorTypes tdpara_W( void )
{
/*
 * Input variables
 */
  float *a00, *v10, *v01;

/*
 * Retrieve parameters.
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
          DONT_CARE);

  v10 = (float*)NclGetArgValue(
          1,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  v01 = (float*)NclGetArgValue(
          2,
          3,
          NULL,
          NULL,
          NULL,
          NULL,
          NULL,
          DONT_CARE);

  c_tdpara(a00[0],a00[1],a00[2],v10[0],v10[1],v10[2],v01[0],v01[1],v01[2]);

  return(NhlNOERROR);
}


NhlErrorTypes tdclrs_W( void )
{
/*
 *  Definte a variable to store the HLU object identifier.
 */
  NclHLUObj tmp_hlu_obj;

  int *nwid, *ibow, *iofc, *iolc, *ilmt;
  float *shde, *shdr;
  int gkswid, grlist, nid;

/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid =    (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ibow =    (int*)NclGetArgValue(1,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  shde =  (float*)NclGetArgValue(2,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  shdr =  (float*)NclGetArgValue(3,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iofc =    (int*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iolc =    (int*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilmt =    (int*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

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
  c_tdclrs(gkswid, *ibow, *shde, *shdr, *iofc, *iolc, *ilmt);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdgetp_W(void)
{
/*
 *  Get values for tdpack parameters.
 */
  char  *arg1;
  int   numpi, numpf, i;

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
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
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
          NULL,
          DONT_CARE);

  arg1 = NrmQuarkToString(*pname);

/*
 *  Check to see if the parameter name is valid.
 */
  numpf = sizeof(params_f)/sizeof(void *);
  numpi = sizeof(params_i)/sizeof(void *);
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      goto OK_NAME;
    }
  }
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      goto OK_NAME;
    }
  }
  NhlPError(NhlFATAL, NhlEUNKNOWN, "tdgetp: unrecognized parameter name");
  return(NhlFATAL);

OK_NAME:
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

/*
 *  Process the parameter if it has an integer value.
 */
  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_tdgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

  NhlPError(NhlFATAL, NhlEUNKNOWN, "tdgetp: impossible to get this message");
  return(NhlFATAL);
}

NhlErrorTypes tdgtrs_W( void )
{
/*
 * Input variables
 */
  int *irst, *ifc1, *ifc2, *ifc3, *ifc4, *ilc1, *ilc2, *iltd;
  float *ustp, *vstp, *wstp;

/*
 * Retrieve parameters.
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  irst =   (int*)NclGetArgValue( 0,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc1 =   (int*)NclGetArgValue( 1,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc2 =   (int*)NclGetArgValue( 2,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc3 =   (int*)NclGetArgValue( 3,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc4 =   (int*)NclGetArgValue( 4,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilc1 =   (int*)NclGetArgValue( 5,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilc2 =   (int*)NclGetArgValue( 6,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iltd =   (int*)NclGetArgValue( 7,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ustp = (float*)NclGetArgValue( 8,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  vstp = (float*)NclGetArgValue( 9,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  wstp = (float*)NclGetArgValue(10,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  c_tdgtrs(*irst,ifc1,ifc2,ifc3,ifc4,ilc1,ilc2,iltd,ustp,vstp,vstp);

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
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int ndims_pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pvalue;

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
          NULL,
          DONT_CARE);

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
           DONT_CARE);

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



/*
 * The pcsetp_W code is based on Fred Clare's wmsetp_W code.
 */

NhlErrorTypes pcsetp_W(void)
{
  char  *arg1;
  int   numpi, numpf, i, j;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"fn","of","FN","OF"};
  char *params_f[] = {"ol","oc","OL","OC"};
/*
 * Input array variables
 */
  NrmQuark *pname;
  int ndims_pname;
  ng_size_t dsizes_pname[NCL_MAX_DIMENSIONS];
  void *pvalue;
  int ndims_pvalue;
  ng_size_t dsizes_pvalue[NCL_MAX_DIMENSIONS];
  NclBasicDataTypes type_pvalue;

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
          NULL,
          DONT_CARE);

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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "pcsetp: unrecognized parameter name");
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
        j = *((int *) pvalue);
        c_pcseti(arg1, j);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "pcsetp: The specified value for the parameter has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_pcsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_pcsetr(arg1, (float) *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "pcsetp: The specified value for the parameter has an invalid type");
    return(NhlFATAL);
  }
  else {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "pcsetp: The specified value for the "
              "parameter has an incorrect type");
    return(NhlFATAL);
  }
}



NhlErrorTypes tdstrs_W( void )
{
/*
 * Input variables
 */
  int *irst, *ifc1, *ifc2, *ifc3, *ifc4, *ilc1, *ilc2, *iltd;
  float *ustp, *vstp, *wstp;

/*
 * Retrieve parameters.
 *
 * Note that any of the pointer parameters can be set to NULL,
 * which implies you don't care about its value.
 */
  irst =   (int*)NclGetArgValue( 0,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc1 =   (int*)NclGetArgValue( 1,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc2 =   (int*)NclGetArgValue( 2,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc3 =   (int*)NclGetArgValue( 3,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ifc4 =   (int*)NclGetArgValue( 4,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilc1 =   (int*)NclGetArgValue( 5,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilc2 =   (int*)NclGetArgValue( 6,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iltd =   (int*)NclGetArgValue( 7,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ustp = (float*)NclGetArgValue( 8,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  vstp = (float*)NclGetArgValue( 9,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  wstp = (float*)NclGetArgValue(10,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  c_tdstrs(*irst,*ifc1,*ifc2,*ifc3,*ifc4,*ilc1,*ilc2,*iltd,
           *ustp,*vstp,*vstp);

  return(NhlNOERROR);
}


NhlErrorTypes tdprpt_W( void )
{
  float *uvw, xy[2];
  ng_size_t dsizes_xy[1];

/*
 * Retrieve parameter.
 */
  uvw = (float*)NclGetArgValue(0,1,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  c_tdprpt(uvw[0], uvw[1], uvw[2], &xy[0], &xy[1]);
  
  dsizes_xy[0] = 2;
  return(NclReturnValue( (void *) xy, 1, dsizes_xy, NULL, NCL_float, 0));
}


NhlErrorTypes tdprpa_W( void )
{
  float *xy_in, xy_out[2];
  ng_size_t dsizes_xy[1];

/*
 * Retrieve parameter.
 */
  xy_in = (float*)NclGetArgValue(0,1,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  c_tdprpa(xy_in[0], xy_in[1], &xy_out[0], &xy_out[1]);
  
  dsizes_xy[0] = 2;
  return(NclReturnValue( (void *) xy_out, 1, dsizes_xy, NULL, NCL_float, 0));
}


NhlErrorTypes tdprpi_W( void )
{
  float *xy_in, xy_out[2];
  ng_size_t dsizes_xy[1];

/*
 * Retrieve parameter.
 */
  xy_in = (float*)NclGetArgValue(0,1,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  c_tdprpi(xy_in[0], xy_in[1], &xy_out[0], &xy_out[1]);
  
  dsizes_xy[0] = 2;
  return(NclReturnValue( (void *) xy_out, 1, dsizes_xy, NULL, NCL_float, 0));
}


NhlErrorTypes tdline_W( void )
{
  int *nwid;
  float *uvw1, *uvw2;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =   (int*)NclGetArgValue(0,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvw1 = (float*)NclGetArgValue(1,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvw2 = (float*)NclGetArgValue(2,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdline function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdline(uvw1[0], uvw1[1], uvw1[2], uvw2[0], uvw2[1], uvw2[2]);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdlnpa_W( void )
{
  int *nwid;
  float *uvw1, *uvw2;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =   (int*)NclGetArgValue(0,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvw1 = (float*)NclGetArgValue(1,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvw2 = (float*)NclGetArgValue(2,3,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdlnpa function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdlnpa(uvw1[0], uvw1[1], uvw2[0], uvw2[1]);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdcurv_W( void )
{
  int *nwid, *iarh;
  float *ucrv, *vcrv, *wcrv, *arhl, *arhw;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;
  ng_size_t ncrv;
  int incrv;
  ng_size_t dsizes_ucrv[1];
  ng_size_t dsizes_vcrv[1];
  ng_size_t dsizes_wcrv[1];

/*
 * Retrieve parameters.
 */
  nwid   =   (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ucrv   = (float*)NclGetArgValue(1,7,NULL,dsizes_ucrv,NULL,NULL,NULL,DONT_CARE);
  vcrv   = (float*)NclGetArgValue(2,7,NULL,dsizes_vcrv,NULL,NULL,NULL,DONT_CARE);
  wcrv   = (float*)NclGetArgValue(3,7,NULL,dsizes_wcrv,NULL,NULL,NULL,DONT_CARE);
  iarh   =   (int*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  arhl   = (float*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  arhw   = (float*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  if(dsizes_ucrv[0] != dsizes_vcrv[0] || dsizes_ucrv[0] != dsizes_wcrv[0]) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdcurv: ucurv, vcurv, and wcurv must be the same length");
    return(NhlFATAL);
  }
  ncrv = dsizes_ucrv[0];

/*
 * Test dimension sizes. 
 */
  if(ncrv > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdcurv: the length of the input arrays are > INT_MAX");
    return(NhlFATAL);
  }
  incrv = (int) ncrv;

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdcurv function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  NGCALLF(tdcurv,TDCURV)(ucrv, vcrv, wcrv, &incrv, iarh, arhl, arhw);

  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdgrds_W( void )
{
  int *nwid, *igrt, *ihid;
  float *uvwmin, *uvwmax, *uvwstp;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid   =   (int*)NclGetArgValue(0,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmin = (float*)NclGetArgValue(1,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmax = (float*)NclGetArgValue(2,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwstp = (float*)NclGetArgValue(3,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  igrt   =   (int*)NclGetArgValue(4,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ihid   =   (int*)NclGetArgValue(5,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdgrds function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdgrds(uvwmin[0], uvwmin[1], uvwmin[2], uvwmax[0], uvwmax[1], uvwmax[2],
           uvwstp[0], uvwstp[1], uvwstp[2], *igrt, *ihid);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdgrid_W( void )
{
  int *nwid, *noxs, *noys, *igrd;
  float *xbeg, *xstp, *ybeg, *ystp;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =   (int*)NclGetArgValue(0,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  xbeg = (float*)NclGetArgValue(1,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  xstp = (float*)NclGetArgValue(2,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  noxs =   (int*)NclGetArgValue(3,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ybeg = (float*)NclGetArgValue(4,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ystp = (float*)NclGetArgValue(5,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  noys =   (int*)NclGetArgValue(6,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  igrd =   (int*)NclGetArgValue(7,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdgrid function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdgrid(*xbeg, *xstp, *noxs, *ybeg, *ystp, *noys, *igrd);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdlbls_W( void )
{
  int *nwid, *ipck;
  float *uvwmn, *uvwmx;
  NrmQuark *uvwn, *uvwi;
  char *cuvwn0, *cuvwi0, *cuvwn1, *cuvwi1, *cuvwn2, *cuvwi2;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid  =    (int*)NclGetArgValue(0,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmn =  (float*)NclGetArgValue(1,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmx =  (float*)NclGetArgValue(2,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwn  = (NrmQuark*)NclGetArgValue(3,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwi  = (NrmQuark*)NclGetArgValue(4,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ipck  =    (int*)NclGetArgValue(5,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  cuvwn0 = NrmQuarkToString(uvwn[0]);
  cuvwi0 = NrmQuarkToString(uvwi[0]);
  cuvwn1 = NrmQuarkToString(uvwn[1]);
  cuvwi1 = NrmQuarkToString(uvwi[1]);
  cuvwn2 = NrmQuarkToString(uvwn[2]);
  cuvwi2 = NrmQuarkToString(uvwi[2]);
/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdlbls function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdlbls(uvwmn[0], uvwmn[1], uvwmn[2], uvwmx[0], uvwmx[1], uvwmx[2],
           cuvwn0, cuvwn1, cuvwn2, cuvwi0, cuvwi1, cuvwi2, *ipck);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdlbla_W( void )
{
  int *nwid, *iaxs;
  float *xat, *yat, *angd;
  NrmQuark *ilbl, *nlbl;
  char *cilbl, *cnlbl;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =    (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iaxs =    (int*)NclGetArgValue(1,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ilbl = (NrmQuark*)NclGetArgValue(2,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  nlbl = (NrmQuark*)NclGetArgValue(3,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  xat  =  (float*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  yat  =  (float*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  angd =  (float*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  cilbl = NrmQuarkToString(*ilbl);
  cnlbl = NrmQuarkToString(*nlbl);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdlbla function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdlbla(*iaxs, cilbl, cnlbl, xat[0], xat[1], yat[0], yat[1], *angd);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdplch_W( void )
{
  int *nwid;
  float *xpos, *ypos, *size, *angd, *cntr;
  NrmQuark *chrs;
  char *cchrs;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =      (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  xpos =    (float*)NclGetArgValue(1,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  ypos =    (float*)NclGetArgValue(2,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  chrs = (NrmQuark*)NclGetArgValue(3,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  size =    (float*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  angd =    (float*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  cntr =    (float*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  cchrs = NrmQuarkToString(*chrs);

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdplch function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tdplch(*xpos, *ypos, cchrs, *size, *angd, *cntr);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tddtri_W( void )
{
  int *nwid, *ntri, *itwk, imtri;
  ng_size_t mtri, dsizes_rtri[2];
  float *rtri;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

/*
 * Retrieve parameters.
 */
  nwid =   (int*)NclGetArgValue(0,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtri = (float*)NclGetArgValue(1,4,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue(2,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  itwk =   (int*)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tddtri: the rightmost dimension of rtri must be 10");
    return(NhlFATAL);
  }
/*
 * Test dimension sizes. 
 */
  if(mtri > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tddtri: the leftmost dimension of rtri is greater than INT_MAX");
    return(NhlFATAL);
  }
  imtri = (int) mtri;

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;

/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist, NhlNwkGksWorkId, &gkswid);
  NhlGetValues(nid, grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tddtri function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);
  c_tddtri(rtri, imtri, ntri, itwk);
  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}


NhlErrorTypes tdstri_W( void )
{
  float *u, *v, *w, *rtri;
  ng_size_t nu, nv, mtri;
  int inu, inv, *ntri, imtri, *irst;
  ng_size_t dsizes_u[1];
  ng_size_t dsizes_v[1];
  ng_size_t dsizes_w[2];
  ng_size_t dsizes_rtri[2];
/*
 * Retrieve parameters.
 */
  u    = (float*)NclGetArgValue(0,6,NULL,dsizes_u,NULL,NULL,NULL,DONT_CARE);
  v    = (float*)NclGetArgValue(1,6,NULL,dsizes_v,NULL,NULL,NULL,DONT_CARE);
  w    = (float*)NclGetArgValue(2,6,NULL,dsizes_w,NULL,NULL,NULL,DONT_CARE);
  rtri = (float*)NclGetArgValue(3,6,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue(4,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  irst =   (int*)NclGetArgValue(5,6,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdstri: the second dimension of rtri must be 10");
    return(NhlFATAL);
  }

  nu = dsizes_u[0];
  nv = dsizes_v[0];

  if(dsizes_w[0] != nv || dsizes_w[1] != nu) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdstri: the dimensions of w must be nv x nu");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if( (nu > INT_MAX) || (nv > INT_MAX) || (mtri > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdstri: one or more input arrays sizes is greater than INT_MAX");
      return(NhlFATAL);
  }
  inu   = (int) nu;
  inv   = (int) nv;
  imtri = (int) mtri;

  NGCALLF(tdstri,TDSTRI)(u, &inu, v, &inv, w, &inu, rtri, &imtri, ntri, irst);

  if(*ntri == imtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdstri: triangle list overflow");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
}


NhlErrorTypes tditri_W( void )
{
  float *u, *v, *w, *f, *fiso, *rtri;
  ng_size_t nu, nv, nw, mtri;
  int inu, inv, inw, *ntri, imtri, *irst;
  ng_size_t dsizes_u[1];
  ng_size_t dsizes_v[1];
  ng_size_t dsizes_w[1];
  ng_size_t dsizes_f[3];
  ng_size_t dsizes_rtri[2];
/*
 * Retrieve parameters.
 */
  u    = (float*)NclGetArgValue( 0,8,NULL,dsizes_u,NULL,NULL,NULL,DONT_CARE);
  v    = (float*)NclGetArgValue( 1,8,NULL,dsizes_v,NULL,NULL,NULL,DONT_CARE);
  w    = (float*)NclGetArgValue( 2,8,NULL,dsizes_w,NULL,NULL,NULL,DONT_CARE);
  f    = (float*)NclGetArgValue( 3,8,NULL,dsizes_f,NULL,NULL,NULL,DONT_CARE);
  fiso = (float*)NclGetArgValue( 4,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtri = (float*)NclGetArgValue( 5,8,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue( 6,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  irst =   (int*)NclGetArgValue( 7,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tditri: the second dimension of ntri must be 10");
    return(NhlFATAL);
  }

  nu = dsizes_u[0];
  nv = dsizes_v[0];
  nw = dsizes_w[0];
  
  if(dsizes_f[0] != nw || dsizes_f[1] != nv || dsizes_f[2] != nu) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tditri: the dimensions of f must be nw x nv x nu");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((nu > INT_MAX) || (nv > INT_MAX) || (nw > INT_MAX) || (mtri > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tditri: one or more input arrays sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  inu   = (int) nu;
  inv   = (int) nv;
  inw   = (int) nw;
  imtri = (int) mtri;

  NGCALLF(tditri,TDITRI)(u,&inu,v,&inv,w,&inw,f,&inu,&inv,fiso,rtri,&imtri,
			 ntri,irst);

  if(*ntri == imtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tditri: triangle list overflow");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
}


NhlErrorTypes tdmtri_W( void )
{
  float *uvw, *s, *rtri;
  float *uvwmin, *uvwmax;
  ng_size_t mtri;
  int *imrk, *ntri, imtri, *irst;
  ng_size_t dsizes_rtri[2];
/*
 * Retrieve parameters.
 */
  imrk =     (int*)NclGetArgValue( 0,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvw  =   (float*)NclGetArgValue( 1,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  s    =   (float*)NclGetArgValue( 2,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtri =   (float*)NclGetArgValue( 3,8,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =     (int*)NclGetArgValue( 4,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  irst =     (int*)NclGetArgValue( 5,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmin = (float*)NclGetArgValue( 6,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmax = (float*)NclGetArgValue( 7,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdmtri: the second dimension of ntri must be 10");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if(mtri > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdmtri: mtri is greater than INT_MAX");
    return(NhlFATAL);
  }
  imtri = (int) mtri;

  c_tdmtri(*imrk, uvw[0], uvw[1], uvw[2], *s, rtri, imtri, ntri, *irst, 
           uvwmin[0], uvwmin[1], uvwmin[2], 
           uvwmax[0], uvwmax[1], uvwmax[2]);

  if(*ntri == imtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdmtri: triangle list overflow");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
}


NhlErrorTypes tdttri_W( void )
{
  float *ucra, *vcra, *wcra, *uvwmin, *uvwmax;
  float *rmrk, *smrk, *rtri;
  ng_size_t mtri, ncra;
  int *imrk, *ntri, *irst, imtri, incra;
  ng_size_t dsizes_rtri[2];
  ng_size_t dsizes_ucra[1];
  ng_size_t dsizes_vcra[1];
  ng_size_t dsizes_wcra[1];
/*
 * Retrieve parameters.
 */
  ucra = (float*)NclGetArgValue( 0,11,NULL,dsizes_ucra,NULL,NULL,NULL,DONT_CARE);
  vcra = (float*)NclGetArgValue( 1,11,NULL,dsizes_vcra,NULL,NULL,NULL,DONT_CARE);
  wcra = (float*)NclGetArgValue( 2,11,NULL,dsizes_wcra,NULL,NULL,NULL,DONT_CARE);

  imrk =   (int*)NclGetArgValue( 3,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rmrk = (float*)NclGetArgValue( 4,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  smrk = (float*)NclGetArgValue( 5,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  rtri = (float*)NclGetArgValue( 6,11,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue( 7,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  irst =   (int*)NclGetArgValue( 8,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  uvwmin = (float*)NclGetArgValue( 9,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  uvwmax = (float*)NclGetArgValue(10,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdttri: the second dimension of ntri must be 10");
    return(NhlFATAL);
  }

  ncra = dsizes_ucra[0];
  if(dsizes_vcra[0] != ncra || dsizes_wcra[0] != ncra) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdttri: ucra, vcra, and wcra must all be the same length");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if((mtri > INT_MAX) || (ncra > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tddtri: one or more input arrays sizes is greater than INT_MAX");
    return(NhlFATAL);
  }
  imtri = (int) mtri;
  incra = (int) ncra;

  NGCALLF(tdttri,TDTTRI)(ucra, vcra, wcra, &incra, imrk, rmrk, smrk, rtri, 
                         &imtri, ntri, irst, 
                         &uvwmin[0], &uvwmin[1], &uvwmin[2],
			 &uvwmax[0], &uvwmax[1], &uvwmax[2]);

  if(*ntri == imtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdttri: triangle list overflow");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
}


NhlErrorTypes tdctri_W( void )
{
  float *rtri, *rcut;
  ng_size_t mtri;
  int *ntri, *iaxs, imtri;
  ng_size_t dsizes_rtri[2];

/*
 * Retrieve parameters.
 */
  rtri = (float*)NclGetArgValue(0,4,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue(1,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  iaxs =   (int*)NclGetArgValue(2,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rcut = (float*)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdctri: the second dimension of ntri must be 10");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if(mtri > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdctri: mtri is greater than INT_MAX");
    return(NhlFATAL);
  }
  imtri = (int) mtri;

  c_tdctri(rtri, mtri, ntri, *iaxs, *rcut);

  if(*ntri == imtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdctri: triangle list overflow");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
}


NhlErrorTypes tdotri_W( void )
{
  ng_size_t mtri;
  int *ntri, imtri, *iord;
  ng_size_t dsizes_rtri[2];
  ng_size_t dsizes_rtwk[2];
  ng_size_t dsizes_itwk[1];
  float *rtri;
/*
 * Work arrays.
 */
  float *rtwk;
  int *itwk, ret;
/*
 * Retrieve parameters.
 */
  rtri = (float*)NclGetArgValue(0,4,NULL,dsizes_rtri,NULL,NULL,NULL,DONT_CARE);
  ntri =   (int*)NclGetArgValue(1,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rtwk = (float*)NclGetArgValue(2,4,NULL,dsizes_rtwk,NULL,NULL,NULL,DONT_CARE);
  iord =   (int*)NclGetArgValue(3,4,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

  mtri = dsizes_rtri[0];
  if(dsizes_rtri[1] != 10) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdotri: the second dimension of ntri must be 10");
    return(NhlFATAL);
  }

  if(dsizes_rtwk[0] != 2 || dsizes_rtwk[1] != mtri) {
    NhlPError(NhlFATAL, NhlEUNKNOWN, "tdotri: the dimensions of rtwk must be 2 x mtri");
    return(NhlFATAL);
  }

/*
 * Test dimension sizes.
 */
  if(mtri > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdotri: mtri is greater than INT_MAX");
    return(NhlFATAL);
  }
  imtri = (int) mtri;

  itwk = (int*)calloc(mtri,sizeof(int));
  if(itwk == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdotri: Unable to allocate memory for permutation vector");
    return(NhlFATAL);
  }

  NGCALLF(tdotri,TDOTRI)(rtri, &imtri, ntri, rtwk, itwk, iord);

  if(*ntri == mtri) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdotri: triangle list overflow");
    return(NhlFATAL);
  }

  dsizes_itwk[0] = mtri;
  ret = NclReturnValue(itwk,1,dsizes_itwk,NULL,NCL_int,0);
  return(NhlNOERROR);
}


NhlErrorTypes tdsort_W( void )
{
  float *rwrk;
  ng_size_t nwrk;
  int *iwrk, *iord, inwrk;
  ng_size_t dsizes_rwrk[1];
  int ret;
/*
 * Retrieve parameters.
 */
  rwrk = (float*)NclGetArgValue(0,2,NULL,dsizes_rwrk,NULL,NULL,NULL,DONT_CARE);
  iord =   (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  nwrk = dsizes_rwrk[0];

/*
 * Test dimension sizes.
 */
  if(nwrk > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdsort: the length of rwrk is greater than INT_MAX");
    return(NhlFATAL);
  }
  inwrk = (int) nwrk;

  iwrk = (int*)calloc(nwrk,sizeof(int));
  if(iwrk == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdsort: Unable to allocate memory for permutation vector");
    return(NhlFATAL);
  }

  c_tdsort(rwrk, inwrk, *iord, iwrk);

  ret = NclReturnValue(iwrk,1,dsizes_rwrk,NULL,NCL_int,0);
  return(NhlNOERROR);
}


NhlErrorTypes tdez2d_W( void )
{
  float *x, *y, *z, *zp;
  ng_size_t nx, dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t ny, dsizes_y[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_z[NCL_MAX_DIMENSIONS];
  float *rmult, *theta, *phi;
  int *style, *nwid;
  ng_size_t i, j, inx, iny;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid   = (int*)NclGetArgValue(0,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  x      = (float*)NclGetArgValue(1,8, NULL, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y      = (float*)NclGetArgValue(2,8, NULL, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  z      = (float*)NclGetArgValue(3,8, NULL, dsizes_z, NULL,NULL,NULL,DONT_CARE);
  rmult  = (float*)NclGetArgValue(4,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  theta  = (float*)NclGetArgValue(5,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  phi    = (float*)NclGetArgValue(6,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  style  = (int*)NclGetArgValue(7,8,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 * Test dimension sizes.
 */
  nx = dsizes_x[0];
  ny = dsizes_y[0];
  if((nx > INT_MAX) || (ny > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez2d: the length of x and/or y is greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int) nx;
  iny = (int) ny;

/*
 * Check input sizes.
 */
  if( (dsizes_z[0] == nx) && (dsizes_z[1] == ny) ) {
/*
 * Reverse the order of the dimensions.
 */
    zp = (float *) calloc(nx * ny,sizeof(float));
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) { 
        zp[j*nx + i] = z[i*ny + j];
      }
    }
/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
    tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
    nid         = tmp_hlu_obj->hlu.hlu_id;
/*
 * Retrieve the GKS workstation id from the workstation object.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(nid,grlist);
 
/*
 * The following section activates the workstation, calls the 
 * c_tdez2d function, and then deactivates the workstation.
 */
    gactivate_ws (gkswid);
    c_tdez2d(inx,iny,x,y,zp,*rmult,*theta,*phi,*style);
    gdeactivate_ws (gkswid);
    free(zp);
  }
  else { 
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez2d: the dimension sizes of z must be the dimension of x by the dimension of y");
    return(NhlFATAL);
  }

  return(NhlNOERROR);
  
}


NhlErrorTypes tdez3d_W( void )
{
  float *x, *y, *z, *u, *value, *up;
  ng_size_t nx, dsizes_x[NCL_MAX_DIMENSIONS];
  ng_size_t ny, dsizes_y[NCL_MAX_DIMENSIONS];
  ng_size_t nz, dsizes_z[NCL_MAX_DIMENSIONS];
  ng_size_t dsizes_u[NCL_MAX_DIMENSIONS];
  float *rmult, *theta, *phi;
  int *nwid, *style;
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid;
  NclHLUObj tmp_hlu_obj;

  ng_size_t i, j, k, inx, iny, inz;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid  =  (int*)NclGetArgValue(0,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  x     =  (float*)NclGetArgValue(1,10, NULL, dsizes_x, NULL,NULL,NULL,DONT_CARE);
  y     =  (float*)NclGetArgValue(2,10, NULL, dsizes_y, NULL,NULL,NULL,DONT_CARE);
  z     =  (float*)NclGetArgValue(3,10, NULL, dsizes_z, NULL,NULL,NULL,DONT_CARE);
  u     =  (float*)NclGetArgValue(4,10, NULL, dsizes_u, NULL,NULL,NULL,DONT_CARE);
  value = (float*)NclGetArgValue(5,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rmult = (float*)NclGetArgValue(6,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  theta = (float*)NclGetArgValue(7,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  phi   = (float*)NclGetArgValue(8,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  style = (int*)NclGetArgValue(9,10,NULL,NULL,NULL,NULL,NULL,DONT_CARE);

/*
 * Test dimension sizes.
 */
  nx = dsizes_x[0];
  ny = dsizes_y[0];
  nz = dsizes_z[0];
  if((nx > INT_MAX) || (ny > INT_MAX) || (nz > INT_MAX)) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez3d: the length of x, y and/or z is greater than INT_MAX");
    return(NhlFATAL);
  }
  inx = (int) nx;
  iny = (int) ny;
  inz = (int) nz;

/*
 * Check input sizes.
 */
  if( (dsizes_u[0] == nx) && (dsizes_u[1] == ny) && (dsizes_u[2] == nz) ) {
/*
 * Reverse the order of the dimensions.
 */
    up = (float *) calloc(nx*ny*nz,sizeof(float));
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) { 
        for (k = 0; k < nz; k++) { 
          up[nx*ny*k + j*nx + i] = 
            u[i*nz*ny + nz*j + k];
        }
      }
    }

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
    tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
    nid         = tmp_hlu_obj->hlu.hlu_id;
 
/*
 * Retrieve the GKS workstation id from the workstation object.
 */
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
    NhlGetValues(nid,grlist);

/*
 * The following section activates the workstation, calls the 
 * c_tdez3d function, and then deactivates the workstation.
 */
    gactivate_ws (gkswid);
    c_tdez3d(nx,ny,nz,x,y,z,up,*value,
             *rmult,*theta,*phi,*style);
    gdeactivate_ws (gkswid);
    free(up);
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez3d: the dimension sizes of u must be the dimension of x by the dimension of y by the dimension of z");
    return(NhlFATAL);
  }
  return(NhlNOERROR);
  
}


NhlErrorTypes tdez1d_W( void )
{
  int *nwid, *imrk, *style;
  float *x, *y, *z, *rmrk, *smrk, *rmult, *theta, *phi;
  ng_size_t dsizes_x[1];
  ng_size_t dsizes_y[1];
  ng_size_t dsizes_z[1];
/*
 * Variables for retrieving workstation information.
 */
  int grlist, gkswid, nid, x0;
  NclHLUObj tmp_hlu_obj;
/*
 * Retrieve parameters.
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  nwid   =   (int*)NclGetArgValue( 0,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  x      = (float*)NclGetArgValue( 1,11,NULL,dsizes_x,NULL,NULL,NULL,DONT_CARE);
  y      = (float*)NclGetArgValue( 2,11,NULL,dsizes_y,NULL,NULL,NULL,DONT_CARE);
  z      = (float*)NclGetArgValue( 3,11,NULL,dsizes_z,NULL,NULL,NULL,DONT_CARE);
  imrk   =   (int*)NclGetArgValue( 4,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rmrk   = (float*)NclGetArgValue( 5,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  smrk   = (float*)NclGetArgValue( 6,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  rmult  = (float*)NclGetArgValue( 7,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  theta  = (float*)NclGetArgValue( 8,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  phi    = (float*)NclGetArgValue( 9,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
  style  =   (int*)NclGetArgValue(10,11,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
/*
 * Check the input sizes.
 */
  if( dsizes_x[0] != dsizes_y[0] || dsizes_x[0] != dsizes_z[0] ) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez1d: the length of the x, y, and z arrays must be the same");
    return(NhlFATAL);
  }
  if(dsizes_x[0] > INT_MAX) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"tdez1d: dsizes_x[0] = %ld is greater than INT_MAX", dsizes_x[0]);
    return(NhlFATAL);
  }
  x0 = (int) dsizes_x[0];

/*
 *  Determine the NCL identifier for the graphic object in nid.
 */
  tmp_hlu_obj = (NclHLUObj) _NclGetObj(*nwid);
  nid         = tmp_hlu_obj->hlu.hlu_id;
/*
 * Retrieve the GKS workstation id from the workstation object.
 */
  grlist = NhlRLCreate(NhlGETRL);
  NhlRLClear(grlist);
  NhlRLGetInteger(grlist,NhlNwkGksWorkId,&gkswid);
  NhlGetValues(nid,grlist);
 
/*
 * The following section activates the workstation, calls the 
 * tdez1d function, and then deactivates the workstation.
 */
  gactivate_ws (gkswid);

  NGCALLF(tdez1d,TDEZ1D)(&x0,x,y,z,imrk,rmrk,smrk,rmult,theta,phi,style);

  gdeactivate_ws (gkswid);

  return(NhlNOERROR);
}

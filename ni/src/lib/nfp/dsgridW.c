#include <string.h>
#include <stdio.h>
/*
* The following are the required NCAR Graphics include files.
* They should be located in ${NCARG_ROOT}/include
*/
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/NclBuiltInSupport.h>
#include <ncarg/gks.h>
#include <ncarg/ngmath.h>

NhlErrorTypes dsgrid2s_W( void )
{
  int ier = 0;
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  float *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  float *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  float *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS];
  int i;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
/*
 * Retrieve parameters
 */

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             5,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (float*)NclGetArgValue(
                             1,
                             5,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (float*)NclGetArgValue(
                             2,
                             5,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);
  xo = (float*)NclGetArgValue(
                             3,
                             5,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (float*)NclGetArgValue(
                             4,
                             5,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);

/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dsgrid2s function.
 */
    zo = (float*)c_dsgrid2s (dsizes_x[0],x,y,z,dsizes_xo[0],dsizes_yo[0],xo,yo,&ier);
    if(!ier) {
/*
 * Returns value to NCL
 */
      ndims_zo = 2;
      dsizes_zo[0] = dsizes_xo[0];
      dsizes_zo[1] = dsizes_yo[0];
      if(ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"dsgrid2s: ier = %d", ier);
      }
      return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_float,0));
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: ier = %d", ier);
      return(NhlFATAL);
    }
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2s: the dimension sizes of parameters x, y and z must be identical");
    return(NhlFATAL);
  }
  
}

NhlErrorTypes dsgrid2d_W( void )
{
  int ier = 0;
  double *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  double *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  double *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  double *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  double *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  double *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS];
  int i;
/*
 * Retrieve parameters
 */

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             5,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (double*)NclGetArgValue(
                             1,
                             5,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (double*)NclGetArgValue(
                             2,
                             5,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  xo = (double*)NclGetArgValue(
                             3,
                             5,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (double*)NclGetArgValue(
                             4,
                             5,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);

/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dsgrid2d function
 */
    zo = (double*)c_dsgrid2d (dsizes_x[0],x,y,z,dsizes_xo[0],dsizes_yo[0],xo,yo,&ier);
    if(!ier) {
/*
 * Returns value to NCL
 */
      ndims_zo = 2;
      dsizes_zo[0] = dsizes_xo[0];
      dsizes_zo[1] = dsizes_yo[0];
      if(ier) {
        NhlPError(NhlWARNING,NhlEUNKNOWN,"dsgrid2d: ier = %d", ier);
      }
      return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_double,0));
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: ier = %d", ier);
      return(NhlFATAL);
    }
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid2d: the dimension sizes of parameters x, y and z must be identical");
    return(NhlFATAL);
  }
  
}

NhlErrorTypes dsgrid3s_W( void )
{
  int ier = 0;
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  float *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS], has_missing_u;
  float *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  float *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  float *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS], has_missing_zo;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  float *uo;
  int ndims_uo,dsizes_uo[NCL_MAX_DIMENSIONS];
  int i;
/*
 * Retrieve parameters
 */

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             7,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (float*)NclGetArgValue(
                             1,
                             7,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (float*)NclGetArgValue(
                             2,
                             7,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  u = (float*)NclGetArgValue(
                             3,
                             7,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             2);

  xo = (float*)NclGetArgValue(
                             4,
                             7,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (float*)NclGetArgValue(
                             5,
                             7,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);
  zo = (float*)NclGetArgValue(
                             6,
                             7,
                             &ndims_zo,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0]) &&
      (dsizes_x[0] == dsizes_z[0]) &&
      (dsizes_x[0] == dsizes_u[0])) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_u) {
       for( i = 0; i < dsizes_u[0]; i++ ) {
         if(u[i] == missing_u.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: u cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_zo) {
       for( i = 0; i < dsizes_zo[0]; i++ ) {
         if(zo[i] == missing_zo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: zo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dsgrid3s function.
 */
    uo = (float*)c_dsgrid3s(dsizes_x[0],x,y,z,u,dsizes_xo[0],dsizes_yo[0],dsizes_zo[0],xo,yo,zo,&ier);
    if(!ier) {
/*
 * Returns value to NCL
 */
      ndims_uo = 3;
      dsizes_uo[0] = dsizes_xo[0];
      dsizes_uo[1] = dsizes_yo[0];
      dsizes_uo[2] = dsizes_zo[0];
      return(NclReturnValue((void*)uo,ndims_uo,dsizes_uo,NULL,NCL_float,0));
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: ier = %d", ier);
      return(NhlFATAL);
    }
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3s: the dimension sizes of parameters x, y, z, and u must be identical");
    return(NhlFATAL);
  }
  
}

NhlErrorTypes dsgrid3d_W( void )
{
  int ier = 0;
  double *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  double *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  double *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  double *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS], has_missing_u;
  double *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  double *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  double *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS], has_missing_zo;
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  double *uo;
  int ndims_uo,dsizes_uo[NCL_MAX_DIMENSIONS];
  int i;
/*
 * Retrieve parameters
 */

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             7,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (double*)NclGetArgValue(
                             1,
                             7,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (double*)NclGetArgValue(
                             2,
                             7,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  u = (double*)NclGetArgValue(
                             3,
                             7,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             2);

  xo = (double*)NclGetArgValue(
                             4,
                             7,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (double*)NclGetArgValue(
                             5,
                             7,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);

  zo = (double*)NclGetArgValue(
                             6,
                             7,
                             &ndims_zo,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0]) &&
      (dsizes_x[0] == dsizes_z[0]) &&
      (dsizes_x[0] == dsizes_u[0])) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_u) {
       for( i = 0; i < dsizes_u[0]; i++ ) {
         if(u[i] == missing_u.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: u cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_zo) {
       for( i = 0; i < dsizes_zo[0]; i++ ) {
         if(zo[i] == missing_zo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: zo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dsgrid3d function.
 */
    uo = (double*)c_dsgrid3d(dsizes_x[0],x,y,z,u,dsizes_xo[0],dsizes_yo[0],dsizes_zo[0],xo,yo,zo,&ier);
    if(!ier) {
/*
 * Returns value to NCL
 */
      ndims_uo = 3;
      dsizes_uo[0] = dsizes_xo[0];
      dsizes_uo[1] = dsizes_yo[0];
      dsizes_uo[2] = dsizes_zo[0];
      return(NclReturnValue((void*)uo,ndims_uo,dsizes_uo,NULL,NCL_double,0));
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: ier = %d", ier);
      return(NhlFATAL);
    }
  }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dsgrid3d: the dimension sizes of parameters x, y, z, and u must be identical");
    return(NhlFATAL);
  }
  
}

NhlErrorTypes dssetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"shd", "SHD"};
  char *params_f[] = {"dmv", "dmx", "exp", "DMV", "DMX", "EXP"};
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
              "dssetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "dssetp: unrecognized parameter name");
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
        c_dsseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_dssetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_dssetrd(arg1, *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((string *) pvalue));
        c_dssetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an invalid type");
    return(NhlFATAL);
  }
}

NhlErrorTypes dsgetp_W(void)
{
/*
 *  Get values for dsgrid parameters.
 */

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;
  string *pvalue, *qvalue;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"shd", "SHD"};
  char *params_f[] = {"dmv", "dmx", "exp", "DMV", "DMX", "EXP"};
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
              "dsgetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "dssetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_dsgeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_dsgetr(arg1, fval);
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
             "dsgetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_dsgetc(arg1, cval);
      qvalue = (string *) calloc(1,sizeof(string));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
}

NhlErrorTypes dspnt2s_W( void )
{
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  float *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  float *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  float *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  int i, ier = 0;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             6,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (float*)NclGetArgValue(
                             1,
                             6,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (float*)NclGetArgValue(
                             2,
                             6,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  xo = (float*)NclGetArgValue(
                             3,
                             6,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (float*)NclGetArgValue(
                             4,
                             6,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);
  zo = (float*)NclGetArgValue(
                             5,
                             6,
                             &ndims_zo,
                             dsizes_zo,
                             NULL,
                             NULL,
                             NULL,
                             2);

/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if(dsizes_x[0] == dsizes_y[0] && dsizes_x[0] == dsizes_z[0] &&
      dsizes_xo[0] == dsizes_yo[0]) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dspnt2s function.
 */
     c_dspnt2s(dsizes_x[0],x,y,z,dsizes_xo[0],xo,yo,zo,&ier);
     if(!ier) {
       return(NhlNOERROR);
     }
     else {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: ier = %d", ier);
       return(NhlFATAL);
     }
   }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2s: the dimension sizes of parameters x, y, and z, and  xo and yo must be identical");
    return(NhlFATAL);
  }
}


NhlErrorTypes dspnt2d_W( void )
{
  double *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  double *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  double *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  double *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  double *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  double *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  int i, ier = 0;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             6,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (double*)NclGetArgValue(
                             1,
                             6,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (double*)NclGetArgValue(
                             2,
                             6,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  xo = (double*)NclGetArgValue(
                             3,
                             6,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (double*)NclGetArgValue(
                             4,
                             6,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);
  zo = (double*)NclGetArgValue(
                             5,
                             6,
                             &ndims_zo,
                             dsizes_zo,
                             NULL,
                             NULL,
                             NULL,
                             2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if(dsizes_x[0] == dsizes_y[0] && dsizes_x[0] == dsizes_z[0] &&
      dsizes_xo[0] == dsizes_yo[0]) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dspnt2d function.
 */
     c_dspnt2d(dsizes_x[0],x,y,z,dsizes_xo[0],xo,yo,zo,&ier);
     if(!ier) {
       return(NhlNOERROR);
     }
     else {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: ier = %d", ier);
       return(NhlFATAL);
     }
   }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt2d: the dimension sizes of parameters x, y, and z, and  xo and yo must be identical");
    return(NhlFATAL);
  }
}


NhlErrorTypes dspnt3s_W( void )
{
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  float *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS], has_missing_u;
  float *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  float *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  float *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS], has_missing_zo;
  float *uo;
  int ndims_uo,dsizes_uo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  int i, ier = 0;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(
                             0,
                             8,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (float*)NclGetArgValue(
                             1,
                             8,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (float*)NclGetArgValue(
                             2,
                             8,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  u = (float*)NclGetArgValue(
                             3,
                             8,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             2);

  xo = (float*)NclGetArgValue(
                             4,
                             8,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (float*)NclGetArgValue(
                             5,
                             8,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);
  zo = (float*)NclGetArgValue(
                             6,
                             8,
                             &ndims_zo,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             2);

  uo = (float*)NclGetArgValue(
                             7,
                             8,
                             &ndims_uo,
                             dsizes_uo,
                             NULL,
                             NULL,
                             NULL,
                             2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if(dsizes_x[0] == dsizes_y[0] && dsizes_x[0] == dsizes_z[0] &&
      dsizes_x[0] == dsizes_u[0] && dsizes_xo[0] == dsizes_yo[0] &&
      dsizes_xo[0] == dsizes_zo[0]) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_u) {
       for( i = 0; i < dsizes_u[0]; i++ ) {
         if(u[i] == missing_u.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: u cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_zo) {
       for( i = 0; i < dsizes_zo[0]; i++ ) {
         if(zo[i] == missing_zo.floatval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: zo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dspnt3s function.
 */
     c_dspnt3s(dsizes_x[0],x,y,z,u,dsizes_xo[0],xo,yo,zo,uo,&ier);
     if(!ier) {
       return(NhlNOERROR);
     }
     else {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: ier = %d", ier);
       return(NhlFATAL);
     }
   }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3s: the dimension sizes of parameters x, y, and z, and  xo and yo must be identical");
    return(NhlFATAL);
  }
}



NhlErrorTypes dspnt3d_W( void )
{
  double *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  double *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  double *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  double *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS], has_missing_u;
  double *xo;
  int ndims_xo,dsizes_xo[NCL_MAX_DIMENSIONS], has_missing_xo;
  double *yo;
  int ndims_yo,dsizes_yo[NCL_MAX_DIMENSIONS], has_missing_yo;
  double *zo;
  int ndims_zo,dsizes_zo[NCL_MAX_DIMENSIONS], has_missing_zo;
  double *uo;
  int ndims_uo,dsizes_uo[NCL_MAX_DIMENSIONS];
  NclScalar missing_x, missing_y, missing_z, missing_u;
  NclScalar missing_xo, missing_yo, missing_zo;
  int i, ier = 0;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(
                             0,
                             8,
                             &ndims_x,
                             dsizes_x,
                             &missing_x,
                             &has_missing_x,
                             NULL,
                             2);
  y = (double*)NclGetArgValue(
                             1,
                             8,
                             &ndims_y,
                             dsizes_y,
                             &missing_y,
                             &has_missing_y,
                             NULL,
                             2);
  z = (double*)NclGetArgValue(
                             2,
                             8,
                             &ndims_z,
                             dsizes_z,
                             &missing_z,
                             &has_missing_z,
                             NULL,
                             2);

  u = (double*)NclGetArgValue(
                             3,
                             8,
                             &ndims_u,
                             dsizes_u,
                             &missing_u,
                             &has_missing_u,
                             NULL,
                             2);

  xo = (double*)NclGetArgValue(
                             4,
                             8,
                             &ndims_xo,
                             dsizes_xo,
                             &missing_xo,
                             &has_missing_xo,
                             NULL,
                             2);
  yo = (double*)NclGetArgValue(
                             5,
                             8,
                             &ndims_yo,
                             dsizes_yo,
                             &missing_yo,
                             &has_missing_yo,
                             NULL,
                             2);
  zo = (double*)NclGetArgValue(
                             6,
                             8,
                             &ndims_zo,
                             dsizes_zo,
                             &missing_zo,
                             &has_missing_zo,
                             NULL,
                             2);

  uo = (double*)NclGetArgValue(
                             7,
                             8,
                             &ndims_uo,
                             dsizes_uo,
                             NULL,
                             NULL,
                             NULL,
                             2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if(dsizes_x[0] == dsizes_y[0] && dsizes_x[0] == dsizes_z[0] &&
      dsizes_x[0] == dsizes_u[0] && dsizes_xo[0] == dsizes_yo[0] &&
      dsizes_xo[0] == dsizes_zo[0]) {
/*
 * Check for missing values. 
 */
     if(has_missing_x) {
       for( i = 0; i < dsizes_x[0]; i++ ) {
         if(x[i] == missing_x.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: x cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_y) {
       for( i = 0; i < dsizes_y[0]; i++ ) {
         if(y[i] == missing_y.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: y cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_z) {
       for( i = 0; i < dsizes_z[0]; i++ ) {
         if(z[i] == missing_z.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: z cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_u) {
       for( i = 0; i < dsizes_u[0]; i++ ) {
         if(u[i] == missing_u.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: u cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_xo) {
       for( i = 0; i < dsizes_xo[0]; i++ ) {
         if(xo[i] == missing_xo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: xo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_yo) {
       for( i = 0; i < dsizes_yo[0]; i++ ) {
         if(yo[i] == missing_yo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: yo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
     if(has_missing_zo) {
       for( i = 0; i < dsizes_zo[0]; i++ ) {
         if(zo[i] == missing_zo.doubleval) {
           NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: zo cannot contain any missing values" );
           return(NhlFATAL);
         }
       }
     }
/*
 * The following section calls the c_dspnt3d function.
 */
     c_dspnt3d(dsizes_x[0],x,y,z,u,dsizes_xo[0],xo,yo,zo,uo,&ier);
     if(!ier) {
       return(NhlNOERROR);
     }
     else {
       NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: ier = %d", ier);
       return(NhlFATAL);
     }
   }
  else {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"dspnt3d: the dimension sizes of parameters x, y, and z, and  xo and yo must be identical");
    return(NhlFATAL);
  }
}

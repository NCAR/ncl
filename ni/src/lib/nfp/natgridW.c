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

extern void drwsrfc (int WKID, int nx, int ny, float *x, float *y, float *z,
              float s1, float s2, float s3, int *iwk);
extern void drwvctc (int WKID, int lx, int ly, float *u, float *v);

extern void drwconc (int WKID, int i, int j, float *z);

NhlErrorTypes natgrids_W( void )
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
  NclScalar missing_x, missing_y, missing_z, missing_xo, missing_yo;
  float *zo;
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
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: x cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_y) {
	   for( i = 0; i < dsizes_y[0]; i++ ) {
		 if(y[i] == missing_y.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: y cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_z) {
	   for( i = 0; i < dsizes_z[0]; i++ ) {
		 if(z[i] == missing_z.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: z cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_xo) {
	   for( i = 0; i < dsizes_xo[0]; i++ ) {
		 if(xo[i] == missing_xo.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: xo cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_yo) {
	   for( i = 0; i < dsizes_yo[0]; i++ ) {
		 if(yo[i] == missing_yo.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: yo cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
/*
 * The following section calls the c_natgrids function.
 */
	zo = (float*)c_natgrids (dsizes_x[0],x,y,z,dsizes_xo[0],dsizes_yo[0],xo,yo,&ier);
	if(!ier || (ier >= 4 && ier <= 6)) {
/*
 * Returns value to NCL
 */
	  ndims_zo = 2;
	  dsizes_zo[0] = dsizes_xo[0];
	  dsizes_zo[1] = dsizes_yo[0];
	  if(ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"natgrids: ier = %d", ier);
	  }
	  return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_float,0));
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: ier = %d", ier);
	  return(NhlFATAL);
	}
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"natgrids: the dimension sizes of parameters x, y and z must be identical");
	return(NhlFATAL);
  }
  
}

NhlErrorTypes natgridd_W( void )
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
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: x cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_y) {
	   for( i = 0; i < dsizes_y[0]; i++ ) {
		 if(y[i] == missing_y.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: y cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_z) {
	   for( i = 0; i < dsizes_z[0]; i++ ) {
		 if(z[i] == missing_z.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: z cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_xo) {
	   for( i = 0; i < dsizes_xo[0]; i++ ) {
		 if(xo[i] == missing_xo.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: xo cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_yo) {
	   for( i = 0; i < dsizes_yo[0]; i++ ) {
		 if(yo[i] == missing_yo.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: yo cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
/*
 * The following section calls the c_natgridd function.
 */
	zo = (double*)c_natgridd (dsizes_x[0],x,y,z,dsizes_xo[0],dsizes_yo[0],xo,yo,&ier);
	if(!ier || (ier >= 4 && ier <= 6)) {
/*
 * Returns value to NCL
 */
	  ndims_zo = 2;
	  dsizes_zo[0] = dsizes_xo[0];
	  dsizes_zo[1] = dsizes_yo[0];
	  if(ier) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"natgridd: ier = %d", ier);
	  }
	  return(NclReturnValue((void*)zo,ndims_zo,dsizes_zo,NULL,NCL_double,0));
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: ier = %d", ier);
	  return(NhlFATAL);
	}
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"natgridd: the dimension sizes of parameters x, y and z must be identical");
	return(NhlFATAL);
  }
}

NhlErrorTypes nnsetp_W(void)
{

  char  *arg1, *cval;
  int   numpi, numpf, numpc, i;

/*
 *  List the integer and float parameter names.  To add new ones,
 *  all that needs to be done is add the names to this list.
 */
  char *params_i[] = {"adf", "asc", "dup", "ext", "igr", "non", "rad", 
                      "sdi", "upd", "ADF", "ASC", "DUP", "EXT", "IGR", 
                      "NON", "RAD", "SDI", "UPD"};
/*
 *  The parameters "xas", "yas", and "zas" are not in the following
 *  list, since they are for retrieval only.
 */
  char *params_f[] = {"bI", "bJ", "hor", "magx", "magy", "magz",
                      "nul", "ver", "Bi", "Bj", "HOR", "MAGX", "MAGY", 
                      "MAGZ", "NUL", "VER", "bi", "bj", "BI", "BJ"};
  char *params_c[] = {"alg", "ALG"};

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
              "nnsetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nnsetp: unrecognized parameter name");
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
        c_nnseti(arg1, *((int *) pvalue));
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_float || type_pvalue == NCL_double) {

/*
 *  Process the parameter if it has a float value or double value.
 */
    for (i = 0; i < numpf; i++) {
      if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
        if (type_pvalue == NCL_float) {
          c_nnsetr(arg1, *((float *) pvalue));
          return(NhlNOERROR);
        }
        else if (type_pvalue == NCL_double) {
          c_nnsetrd(arg1, *((double *) pvalue));
          return(NhlNOERROR);
        }
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
  else if (type_pvalue == NCL_string) {

/*
 *  Process the parameter if it has a string value.
 */
    for (i = 0; i < numpc; i++) {
      if (!strncmp(arg1, params_c[i], strlen(params_c[i]))) {
        cval = NrmQuarkToString( *((string *) pvalue));
        c_nnsetc(arg1, cval);
        return(NhlNOERROR);
      }
    }
    NhlPError(NhlFATAL, NhlEUNKNOWN, "The specified value for the parameter "
              "has an incorrect type");
    return(NhlFATAL);
  }
}

NhlErrorTypes nngetp_W(void)
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
  char *params_i[] = {"adf", "asc", "dup", "ext", "igr", "non", "rad", 
                      "sdi", "upd", "ADF", "ASC", "DUP", "EXT", "IGR", 
                      "NON", "RAD", "SDI", "UPD"};
  char *params_f[] = {"bI", "bJ", "hor", "magx", "magy", "magz",
                      "nul", "ver", "xas", "yas", "zas", "Bi", "Bj", 
                      "HOR", "MAGX", "MAGY", "MAGZ", "NUL", "VER", 
                      "bi", "bj", "BI", "BJ", "XAS", "YAS", "ZAS"};
  char *params_c[] = {"alg", "ALG"};

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
              "nngetp: Argument #1 has the wrong number of dimensions.");
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
  NhlPError(NhlFATAL, NhlEUNKNOWN, "nnsetp: unrecognized parameter name");
  return(NhlFATAL);

/*
 *  Process the parameter if it has an integer value.
 */
OK_NAME:  for (i = 0; i < numpi; i++) {
    if (!strncmp(arg1, params_i[i], strlen(params_i[i]))) {
      ival = (int *) calloc(1,sizeof(int));
      c_nngeti(arg1, ival);
      return(NclReturnValue( (void *) ival, 1, &ret_size, NULL, NCL_int, 0));
    }
  }

/*
 *  Process the parameter if it has a float value.
 */
  for (i = 0; i < numpf; i++) {
    if (!strncmp(arg1, params_f[i], strlen(params_f[i]))) {
      fval = (float *) calloc(1,sizeof(float));
      c_nngetr(arg1, fval);
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
             "nngetp: unable to allocate memory for return string");
        return(NhlFATAL);
      }
      c_nngetc(arg1, cval);
      qvalue = (string *) calloc(1,sizeof(string));
      *qvalue = NrmStringToQuark(cval);
      return(NclReturnValue((void *) qvalue, 1, &ret_size, NULL,NCL_string, 1));
    }
  }
}


NhlErrorTypes nngetaspects_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  int dsizes[1];
  float *rtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
  rtmp = (float*)NclMalloc(sizeof(float));
/*
 * The following section allocates the work memory and calls the
 * c_nngetaspects function.
 */
  c_nngetaspects(*i,*j,rtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
	dsizes[0] = 1;
	if(ier) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetaspects: ier = %d", ier);
	}
	return(NclReturnValue((void*)rtmp,1,dsizes,NULL,NCL_float,0));
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetaspects: ier = %d", ier);
	return(NhlFATAL);
  }
}


NhlErrorTypes nngetaspectd_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  int dsizes[1];
  double *dtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
  dtmp = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nngetaspectd function.
 */
  c_nngetaspectd(*i,*j,dtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
	dsizes[0] = 1;
	if(ier) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetaspectd: ier = %d", ier);
	}
	return(NclReturnValue((void*)dtmp,1,dsizes,NULL,NCL_double,0));
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetaspectd: ier = %d", ier);
	return(NhlFATAL);
  }
}


NhlErrorTypes nngetslopes_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  int dsizes[1];
  float *rtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
  rtmp = (float*)NclMalloc(sizeof(float));
/*
 * The following section allocates the work memory and calls the
 * c_nngetslopes function.
 */
  c_nngetslopes(*i,*j,rtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
	dsizes[0] = 1;
	if(ier) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetslopes: ier = %d", ier);
	}
	return(NclReturnValue((void*)rtmp,1,dsizes,NULL,NCL_float,0));
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetslopes: ier = %d", ier);
	return(NhlFATAL);
  }
}


NhlErrorTypes nngetsloped_W( void )
{
  int ier = 0;
  int *i;
  int *j;
  int dsizes[1];
  double *dtmp;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  i = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  j = (int*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
  dtmp = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nngetsloped function.
 */
  c_nngetsloped(*i,*j,dtmp,&ier);
  if(!ier || (ier >= 4 && ier <= 6)) {
	dsizes[0] = 1;
	if(ier) {
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"nngetsloped: ier = %d", ier);
	}
	return(NclReturnValue((void*)dtmp,1,dsizes,NULL,NCL_double,0));
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nngetsloped: ier = %d", ier);
	return(NhlFATAL);
  }
}


NhlErrorTypes drwsrfc_W( void )
{
  int ier = 0;
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS];
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS];
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS];
  float *x1,*y1,*z1;
  int *gkswid;
  int i;
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
  gkswid = (int*)NclGetArgValue(0,7,NULL,NULL,NULL,NULL,NULL,2);
  x = (float*)NclGetArgValue(1,7, &ndims_x, dsizes_x, NULL,NULL,NULL,2);
  y = (float*)NclGetArgValue(2,7, &ndims_y, dsizes_y, NULL,NULL,NULL,2);
  z = (float*)NclGetArgValue(3,7, &ndims_z, dsizes_z, NULL,NULL,NULL,2 );
  x1 = (float*)NclGetArgValue(4,7,NULL,NULL,NULL,NULL,NULL,2);
  y1 = (float*)NclGetArgValue(5,7,NULL,NULL,NULL,NULL,NULL,2);
  z1 = (float*)NclGetArgValue(6,7,NULL,NULL,NULL,NULL,NULL,2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * The following section allocates the work memory and calls the
 * drwsrfc function.
 */
	 iwk = (int*)NclMalloc(2*dsizes_x[0]*dsizes_y[0]*sizeof(int));
	 if( iwk == NULL ) {
	   NhlPError(NhlFATAL,NhlEUNKNOWN,"drwsrfc: Unable to allocate memory for work array");
	   return(NhlFATAL);
	 }
	 drwsrfc(*gkswid,dsizes_x[0],dsizes_y[0],x,y,z,*x1,*y1,*z1,iwk);
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"drwsrfc: the dimension sizes of parameters x, y and z must be identical");
	return(NhlFATAL);
  }
   return(NhlNOERROR);
  
}

NhlErrorTypes drwvctc_W( void )
{
  float *u;
  int ndims_u,dsizes_u[NCL_MAX_DIMENSIONS];
  float *v;
  int ndims_v,dsizes_v[NCL_MAX_DIMENSIONS];
  int i;
  int *gkswid;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  gkswid = (int*)NclGetArgValue(0,3,NULL,NULL,NULL,NULL,NULL,2);
  u = (float*)NclGetArgValue(1,3, &ndims_u, dsizes_u, NULL,NULL,NULL,2);
  v = (float*)NclGetArgValue(2,3, &ndims_v, dsizes_v, NULL,NULL,NULL,2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
   if((dsizes_u[0] == dsizes_v[0])&&(dsizes_u[1] == dsizes_v[1])) {
/*
 * The following section allocates the work memory and calls the
 * drwvctc function.
 */
	 drwvctc(*gkswid,dsizes_u[0],dsizes_u[1],u,v);
  }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"drwvctc: the dimension sizes of parameters u and v must be identical");
	return(NhlFATAL);
  }
   return(NhlNOERROR);
}


NhlErrorTypes drwconc_W( void )
{
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS];
  int i;
  int *gkswid;
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  gkswid = (int*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  z = (float*)NclGetArgValue(1,2, &ndims_z, dsizes_z, NULL,NULL,NULL,2);
/*
 * This is the only dimension size check needed since the function
 * is registered to only accept single dimension parameters.
 */
/*
 * The following section allocates the work memory and calls the
 * drwconc function.
 */
  drwconc(*gkswid,dsizes_z[0],dsizes_z[1],z);
  return(NhlNOERROR);
}


NhlErrorTypes nnpntinits_W( void )
{
  float *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  float *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  float *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  NclScalar missing_x, missing_y, missing_z;
  int i;
/*
 * Retrieve parameters
 */
  x = (float*)NclGetArgValue(
							 0,
							 3,
							 &ndims_x,
							 dsizes_x,
							 &missing_x,
							 &has_missing_x,
							 NULL,
							 2);
  y = (float*)NclGetArgValue(
							 1,
							 3,
							 &ndims_y,
							 dsizes_y,
							 &missing_y,
							 &has_missing_y,
							 NULL,
							 2);
  z = (float*)NclGetArgValue(
							 2,
							 3,
							 &ndims_z,
							 dsizes_z,
							 &missing_z,
							 &has_missing_z,
							 NULL,
							 2);

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
	 if(has_missing_x) {
	   for( i = 0; i < dsizes_x[0]; i++ ) {
		 if(x[i] == missing_x.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: x cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_y) {
	   for( i = 0; i < dsizes_y[0]; i++ ) {
		 if(y[i] == missing_y.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: y cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_z) {
	   for( i = 0; i < dsizes_z[0]; i++ ) {
		 if(z[i] == missing_z.floatval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: z cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
/*
 * The following section allocates the work memory and calls the
 * c_nnpntinits function.
 */
	 c_nnpntinits(dsizes_x[0],x,y,z);
	 return(NhlNOERROR);
   }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinits: the dimension sizes of parameters x, y and z must be identical");
	return(NhlFATAL);
  }
}

NhlErrorTypes nnpntinitd_W( void )
{
  double *x;
  int ndims_x,dsizes_x[NCL_MAX_DIMENSIONS], has_missing_x;
  double *y;
  int ndims_y,dsizes_y[NCL_MAX_DIMENSIONS], has_missing_y;
  double *z;
  int ndims_z,dsizes_z[NCL_MAX_DIMENSIONS], has_missing_z;
  NclScalar missing_x, missing_y, missing_z;
  int i;
/*
 * Retrieve parameters
 */
  x = (double*)NclGetArgValue(
							 0,
							 3,
							 &ndims_x,
							 dsizes_x,
							 &missing_x,
							 &has_missing_x,
							 NULL,
							 2);
  y = (double*)NclGetArgValue(
							 1,
							 3,
							 &ndims_y,
							 dsizes_y,
							 &missing_y,
							 &has_missing_y,
							 NULL,
							 2);
  z = (double*)NclGetArgValue(
							 2,
							 3,
							 &ndims_z,
							 dsizes_z,
							 &missing_z,
							 &has_missing_z,
							 NULL,
							 2);

/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
   if((dsizes_x[0] == dsizes_y[0])&&(dsizes_x[0] == dsizes_z[0])) {
/*
 * Check for missing values. 
 */
	 if(has_missing_x) {
	   for( i = 0; i < dsizes_x[0]; i++ ) {
		 if(x[i] == missing_x.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: x cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_y) {
	   for( i = 0; i < dsizes_y[0]; i++ ) {
		 if(y[i] == missing_y.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: y cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
	 if(has_missing_z) {
	   for( i = 0; i < dsizes_z[0]; i++ ) {
		 if(z[i] == missing_z.doubleval) {
		   NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: z cannot contain any missing values" );
		   return(NhlFATAL);
		 }
	   }
	 }
/*
 * The following section allocates the work memory and calls the
 * c_nnpntinitd function.
 */
	 c_nnpntinitd(dsizes_x[0],x,y,z);
	 return(NhlNOERROR);
   }
  else {
	NhlPError(NhlFATAL,NhlEUNKNOWN,"nnpntinitd: the dimension sizes of parameters x, y and z must be identical");
	return(NhlFATAL);
  }
}

NhlErrorTypes nnpnts_W( void )
{
  float *x;
  float *y;
  float *z;
  int dsizes[1];
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (float*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  y = (float*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
/*
 * The following section allocates the output memory and calls the
 * c_nnpnts function.
 */
  z = (float*)NclMalloc(sizeof(float));
  c_nnpnts(x[0],y[0],&z[0]);
  dsizes[0] = 1;
  return(NclReturnValue((void*)z,1,dsizes,NULL,NCL_float,0));
}


NhlErrorTypes nnpntd_W( void )
{
  double *x;
  double *y;
  double *z;
  int dsizes[1];
/*
 * Retrieve parameters
 */
/*
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value. In this example
 * the type parameter is set to NULL because the function
 * is later registered to only accept floating point numbers.
 */
  x = (double*)NclGetArgValue(0,2,NULL,NULL,NULL,NULL,NULL,2);
  y = (double*)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,2);
  z = (double*)NclMalloc(sizeof(double));
/*
 * The following section allocates the work memory and calls the
 * c_nnpnts function.
 */
  c_nnpntd(*x,*y,z);
  dsizes[0] = 1;
  return(NclReturnValue((void*)z,1,dsizes,NULL,NCL_double,0));
}



NhlErrorTypes nnpntend_W( void )
{
/*
 * The following section allocates the work memory and calls the
 * c_nnpntend function.
 */
  c_nnpntend();
  return(NhlNOERROR);
}


NhlErrorTypes nnpntendd_W( void )
{
/*
 * The following section allocates the work memory and calls the
 * c_nnpntend function.
 */
  c_nnpntendd();
  return(NhlNOERROR);
}


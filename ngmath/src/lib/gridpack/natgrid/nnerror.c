#include <stdio.h>
#include <stdlib.h>

#define  MAX_ERROR 30

void     ErrorLog(int, char *, FILE *, char *);
char     *ErrMsg(int);

extern   int   error_status;

void ErrorHnd(int error, char *func, FILE *efile, char *smsg)
{
  ErrorLog(error, func, efile, smsg);
}

void ErrorLog(int error, char *func, FILE *efile, char *smsg)
{
  if ( (error == 4) || (error == 5) || (error == 6 || error == 28) ) 
  {
     fprintf(efile, "natgrid - warning number %d from %s:\n  %s", 
                  error, func, ErrMsg(error));
     error_status = 0;
  }
  else
  {
     fprintf(efile, "natgrid - error number %d from %s:\n  %s", 
                  error, func, ErrMsg(error));
     error_status = error;
  }
  fprintf(efile,"%s",smsg);
}

char *ErrMsg(int i)
{
  char *rlist;
  const char *err_list[MAX_ERROR] = { 

/* #001 */
    "Insufficient data in gridded region to triangulate.",

/* #002 */
    "Duplicate input data coordinates are not allowed.",

/* #003 */
    "Unable to open file for writing algorithmic data.",

/* #004 */
    "WARNING:  The ratio of vertical to horizontal scales is too large for \n  meaningful gradient estimation.  Rescale the data if gradients are required.",

/* #005 */
    "WARNING:  The ratio of vertical to horizontal scales is too small for\n  meaningful gradient estimation.  Rescale the data if gradients are required.",

/* #006 */
    "WARNING:  The ratio of x-axis breadth to y-axis breadth of this gridded \n  region may be too extreme for good interpolation.  Changing the block \n  proportions, or rescaling the x or y coordinate may be indicated.\n  Gradient calculations have been disabled.",

/* #007 */
    "Unable to allocate storage for ivector.",

/* #008 */
    "Unable to allocate storage for dvector.",

/* #009 */
    "Unable to allocate storage for **imatrix.",

/* #010 */
    "Unable to allocate storage for imatrix[].",

/* #011 */
    "Unable to allocate storage for **fmatrix.",

/* #012 */
    "Unable to allocate storage for fmatrix[].",

/* #013 */
    "Unable to allocate storage for **dmatrix.",

/* #014 */
    "Unable to allocate storage for dmatrix[].",

/* #015 */
    "Unable to allocate storage for raw data.",

/* #016 */
    "Unable to allocate storage for a simplex.",

/* #017 */
    "Unable to allocate storage for temp.",

/* #018 */
    "Unable to allocate storage for neig.",

/* #019 */
    "Slopes have not been computed, set sdi.",

/* #020 */
    "Row argument out of range.",

/* #021 */
    "Column argument out of range.",

/* #022 */
    "Aspects have not been computed, set sdi.",

/* #023 */
    "Parameter name not known.",

/* #024 */
    "Cannot open error file.",

/* #025 */
    "Automatic scaling has been done - aspects will be distorted and \n  consequently are not returned.  Rescale your data manually, or \n  by setting magx, magy, and magz appropriately.",

/* #026 */
    "Automatic scaling has been done - slopes will be distorted and \n  consequently are not returned.  Rescale your data manually, or \n  by setting magx, magy, and magz appropriately.",

/* #027 */
    "Coordinate is outside of the gridded region for a single point interpolation.",

/* #028 */
    "Cannot compute aspects and slopes in conjunction with single point \n  interpolation mode.",

/* #029 */
    "Fortran DOUBLE PRECISION entries are not supported on UNICOS.",

/* #030 */
    "Error number out of range."
   };

  if (i >= MAX_ERROR) {
    rlist = (char *) err_list[29];
  }
  else {
    rlist = (char *) err_list[i-1];
  }
    return (rlist);
}

int ErrMax()
{
 return(MAX_ERROR);
}

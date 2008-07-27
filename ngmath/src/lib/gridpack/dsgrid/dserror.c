/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ngmath.h>
#include "dstypes.h"
#include "dsproto.h"

#define  MAX_ERROR 14

extern   int   ds_error_status;

void DSErrorHnd(int error, char *func, FILE *efile, char *smsg)
{
  DSErrorLog(error, func, efile, smsg);
}

void DSErrorLog(int error, char *func, FILE *efile, char *smsg)
{
  if (error == 14) 
  {
     fprintf(efile, "dsgrid - warning number %d from %s:\n  %s", 
                  error, func, DSErrMsg(error));
     ds_error_status = 0;
  }
  else
  {
     fprintf(efile, "dsgrid - error number %d from %s:\n  %s", 
                  error, func, DSErrMsg(error));
     ds_error_status = error;
  }
  fprintf(efile,"%s",smsg);
}

char *DSErrMsg(int i)
{
  char *rlist;
  const char *error_list[MAX_ERROR] = { 

/* #001 */
    "Error number out of range.",

/* #002 */
    "Insufficient data in gridded region to triangulate.",

/* #003 */
    "Array dimension out of range.",

/* #004 */
    "Parameter name not known.",

/* #005 */
    "Cannot open error file.",

/* #006 */
    "Error allocating memory for input points.",

/* #007 */
    "Fortran DOUBLE PRECISION entries are not supported on UNICOS.",

/* #008 */
    "",

/* #009 */
    "Error allocating memory for array of distances between input points.",

/* #010 */
    "Error allocating memory for weights.",

/* #011 */
    "Error allocating memory for distance ordering vector.",

/* #012 */
    "Error allocating memory for output array.",

/* #013 */
    "Number of input points must be greater than 2.",

/* #014 */
    "No original data values within the specified distance - \n"
    "   interpolated value set to missing value flag."
  };

  if ( (i > 1) && (i <= MAX_ERROR) ) {
    rlist = (char *) error_list[i-1];
  }
  else {
    rlist = (char *) error_list[0];
  }
    return (rlist);
}

int DSErrMax()
{
 return(MAX_ERROR);
}

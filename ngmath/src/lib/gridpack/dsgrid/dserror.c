/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
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

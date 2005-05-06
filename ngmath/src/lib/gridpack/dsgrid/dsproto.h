/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#ifndef MAX
#define MAX(a,b)        (((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)        (((a)<(b))?(a):(b))
#endif

/*
 *  Specify function prototypes.
 */

double   dist_pow(double);
float    dist_pow_s(float);
void     dweights(int);
void     dweights_s(int);
void     sweights(int, double, double, double);
void     sweights_s(int, float, float, float);

double   dotd(DSpointd3, DSpointd3);
float    dot_s(DSpoints3, DSpoints3);
double   ivalue(int, double *, double, double, double);
float    ivalue_s(int, float *, float, float, float);
double   svalue(int, double *, double, double, double);
float    svalue_s(int, float *, float, float, float);
void     dsinit(int, int, int, int, double [], double [], double [], 
                double[], double[], double[], int *);
void     dsinit_s(int, int, int, int, float [], float [], float [], 
                float[], float[], float[], int *);
void     dsgetmem(int, int, int, int, int *);
void     dsgetmem_s(int, int, int, int, int *);
void     dsfreemem();
void     dsfreemem_s();
void     dsdist(int, DSpointd3 [], DSpointd3, double *);
void     dsdist_s(int, DSpoints3 [], DSpoints3, float *);
void     dssortd(int, double [], int []);
void     dssorts(int, float [], int []);
double   dsangd (DSpointd3, DSpointd3, DSpointd3);
float    dsangs (DSpoints3, DSpoints3, DSpoints3);
double   magd (DSpointd3);
float    mags (DSpoints3);
void     DSErrorLog(int, char *, FILE *, char *);
void     DSErrorHnd(int, char *, FILE *, char *);
char     *DSErrMsg(int);
int      DSErrMax();

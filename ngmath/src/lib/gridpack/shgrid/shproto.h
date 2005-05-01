/*
 * $Id: shproto.h,v 1.7 2005-05-01 20:58:23 haley Exp $
 */
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

#include <stddef.h>
#include <stdio.h>
 
/*
 *  Specify all of the function prototypes.
 */
int c_shgetnp(float, float, float, int, float *, float *, float *,
              int, int *); 
float *c_shgrid(int, float [], float [], float [], float [], 
                int, int, int, float [], float [], float [], int*);
void c_shseti(char *, int);
int c_shgeti(char *);

/*
 *  Fortran function macro.  This macro is used to provide the appropriate
 *  system-specific C function name for it to be Fortran callable.
 */
#ifndef NGCALLF

#if defined(F_UPPERCASE)

#define NGCALLF(reg,caps)       caps

#elif defined(F_NO_UNDERSCORES)

#define NGCALLF(reg,caps)       reg
 
#else
#ifdef  __STDC__
#define NGCALLF(reg,caps)       reg##_
#else
#define NGCALLF(reg,caps)       reg/**/_
 
#endif  /* __STDC__ */
#endif	/* F_UPPERCASE */
#endif  /* NGCALLF */

/*
 *  prototypes for Fortran calls.
 */
void NGCALLF(shgrid,SHGRID)(int *, float *, float *, float *, float *, 
                            int *, int *, int *, float *, float *, 
                            float *, float *, int *, float *, int *);
void NGCALLF(shgetnp,SHGETNP)(float *, float *, float *, int *, float *, 
                              float *, float *, int *, int *, float *, 
                              int *, int *);
void NGCALLF(shseti,SHSETI)(char *, int *, size_t);
void NGCALLF(shgeti,SHGETI)(char *, int *, size_t);



/*
 *	$Id: c_ngreop.c,v 1.2 2000-07-12 16:24:50 haley Exp $
 */
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

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

void c_ngreop
#ifdef NeedFuncProto
(
    int wkid,
    int conid,
    int itype,
    char *fname,
    int iopt,
    int *iat,
    float *rat,
    int ncolrs,
    int nstart,
    Gcolr_rep *ctab
)
#else
(wkid,conid,itype,fname,iopt,iat,rat,ncolrs,nstart,ctab)   
    int wkid;
    int conid;
    int itype;
    char *fname;
    int iopt; 
    int *iat; 
    float *rat; 
    int ncolrs; 
    int nstart;
    Gcolr_rep *ctab;
#endif
{
    NGstring fname2;
    int len;

    len = NGSTRLEN(fname);
    fname2 = NGCstrToFstr(fname,len);
    NGCALLF(ngreop,NGREOP)(&wkid,&conid,&itype,fname2,&iopt,iat,rat,&ncolrs,&nstart,ctab,len);
}



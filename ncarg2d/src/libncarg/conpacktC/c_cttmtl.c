/*
 *      $Id: c_cttmtl.c,v 1.1 2003-09-29 23:05:55 kennison Exp $
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
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(cttmtl,CTTMTL)(int*,float*,int*,int*,
                                   int*,int*,int*,
                                   int*,int*,int*,
                                   float*,int*,int*,int*,
                                     int*,int*,int*,int*,
                                     int*,int*,int*,int*);

void c_cttmtl
#ifdef NeedFuncProto
(
    int ntto,
    float *tbuf,
    int mbuf,
    int *nbuf,
    int *ippp,
    int mppp,
    int *nppp,
    int *ippe,
    int mppe,
    int *nppe,
    float *rpnt,
    int mpnt,
    int *npnt,
    int lopn,
    int *iedg,
    int medg,
    int *nedg,
    int loen,
    int *itri,
    int mtri,
    int *ntri,
    int lotn
)
#else
(ntto,tbuf,mbuf,nbuf,ippp,mppp,nppp,ippe,mppe,nppe,rpnt,mpnt,npnt,lopn,
 iedg,medg,nedg,loen,itri,mtri,ntri,lotn)
    int ntto;
    float *tbuf;
    int mbuf;
    int *nbuf;
    int *ippp;
    int mppp;
    int *nppp;
    int *ippe;
    int mppe;
    int *nppe;
    float *rpnt;
    int mpnt;
    int *npnt;
    int lopn;
    int *iedg;
    int medg;
    int *nedg;
    int loen;
    int *itri;
    int mtri;
    int *ntri;
    int lotn;
#endif
{
    NGCALLF(cttmtl,CTTMTL)(&ntto,tbuf,&mbuf,nbuf,
                           ippp,&mppp,nppp,
                           ippe,&mppe,nppe,
                           rpnt,&mpnt,npnt,&lopn,
                           iedg,&medg,nedg,&loen,
                           itri,&mtri,ntri,&lotn);
}

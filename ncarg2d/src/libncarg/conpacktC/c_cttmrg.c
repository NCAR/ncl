/*
 *      $Id: c_cttmrg.c,v 1.1 2003-09-29 23:05:55 kennison Exp $
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

extern void NGCALLF(cttmrg,CTTMRG)(int*,int*,float*,float*,float*,int*,
                                   float*,void (*rtmi_)(),
                                   float*,int*,int*,int*,
                                     int*,int*,int*,int*,
                                     int*,int*,int*,int*);


void c_cttmrg
#ifdef NeedFuncProto
(
    int idim,
    int jdim,
    float *rlat,
    float *rlon,
    float *rdat,
    int *iscr,
    float sval,
    void (*rtmi_) (
            int *idim,
            int *jdim,
            int *iini,
            int *jini,
            int *iino,
            int *jino
            ),
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
(idim,jdim,rlat,rlon,rdat,iscr,sval,rtmi_,rpnt,mpnt,npnt,lopn,
 iedg,medg,nedg,loen,itri,mtri,ntri,lotn)
    int idim;
    int jdim;
    float *rlat;
    float *rlon;
    float *rdat;
    int *iscr;
    float sval;
    void (*rtmi_) ();
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
    NGCALLF(cttmrg,CTTMRG)(&idim,&jdim,rlat,rlon,rdat,iscr,&sval,rtmi_,
                           rpnt,&mpnt,npnt,&lopn,
                           iedg,&medg,nedg,&loen,
                           itri,&mtri,ntri,&lotn);
}

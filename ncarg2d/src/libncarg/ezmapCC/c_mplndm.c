/*
 *      $Id: c_mplndm.c,v 1.1 2001-10-10 02:51:39 haley Exp $
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

extern void NGCALLF(mplndm,MPLNDM)(NGstring,int*,int*,float*,float*,int*,
                                   int*,int*,int*,int (*ulpr_)(),int);

void c_mplndm
#ifdef NeedFuncProto
(
    char  *flnm,
    int    ilvl,
    int   *iama,
    float *xcra,
    float *ycra,
    int    mcra,
    int   *iaai,
    int   *iagi,
    int    mnog,
    int (*ulpr_)(
        float *xcra,
        float *ycra,
	int   *ncra,
	int   *iaai,
	int   *iagi,
	int   *ngpsi
                )
)
#else
(flnm,ilvl,iama,iama,xcra,ycra,mcra,iaai,iagi,mnog,ulpr)
    char  *flnm;
    int    ilvl;
    int   *iama;
    float *xcra;
    float *ycra;
    int    mcra;
    int   *iaai;
    int   *iagi;
    int    mnog;
    int (*ulpr_)();
#endif
{
    int len;
    NGstring flnm_f;
    len=NGSTRLEN(flnm);
    flnm_f=NGCstrToFstr(flnm,len);
    NGCALLF(mplndm,MPLNDM)(flnm_f,&ilvl,iama,xcra,ycra,&mcra,
                           iaai,iagi,&mnog,ulpr_,len);
    return;
}
